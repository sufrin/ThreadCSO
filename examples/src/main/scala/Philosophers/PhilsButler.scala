// Dining Philosophers -- shared-channel solution
// Now compatible with Scala 2.13

import io.threadcso._
import scala.language.postfixOps

object PhilsButler {
  def ManyOne[T](name: String) = N2N[T](name = name, writers = 0, readers = 1)

  val N = 5 // Number of philosophers

  val random = new scala.util.Random

  abstract class Action {}
  case class Pick(who: Int) extends Action
  case class Drop(who: Int) extends Action

  case class Sit(who: Int)
  case class Stand(who: Int)

  // Simulate basic actions
  def Eat = sleep(50 * milliSec)
  def Think = sleep(random.nextInt(80) * milliSec)
  def Pause = sleep(50 * milliSec)

  // channel to report what's happening
  val report = N2NBuf[String](20, 0, 1, "report")

  // Channels to communicate with the butler
  val sit = ManyOne[Sit]("sit")
  val stand = ManyOne[Stand]("stand")

  // A single philosopher
  def Phil(me: Int, left: !![Action], right: !![Action]) = proc(s"Phil $me") {
    repeat {
      Think
      sit ! Sit(me); report ! (s"$me sits"); Pause
      left ! Pick(me); report ! (s"$me picks up left fork"); Pause
      right ! Pick(me); report ! (s"$me picks up right fork"); Pause
      report ! (s"$me eats"); Eat
      left ! Drop(me); report ! (s"$me drops left fork"); Pause
      right ! Drop(me); report ! (s"$me drops right fork"); Pause
      stand ! Stand(me); report ! (s"$me leaves")
    }
    println(s"Phil $me DIED")
  }

  // A single fork
  def Fork(me: Int, left: ??[Action], right: ??[Action]) = proc(s"Fork $me") {
    var owner: String = "Nobody"
    withDebuggerFormat(s"Fork ${me} with $owner") {
      serve {
        (left =?=> { case Pick(x) =>
          owner = s"$x";
          left ? { case Drop(y) => assert(y == x); owner = "nobody" }
        }
          | right =?=> { case Pick(x) =>
            owner = s"$x";
            right ? { case Drop(y) => assert(y == x); owner = "nobody" }
          })
      }
      println(s"FORK $me DIED: owned by ${owner}")
    }
  }

  // The butler
  def Butler(sit: ??[Sit], stand: ??[Stand]) = proc("Butler") {
    val seats = scala.collection.mutable.Set[Int]() // seated
    withDebuggerFormat(s"""Butler: ${seats.mkString(" ")} present""") {
      var seated = 0 // Number currently seated
      serve {
        ((seated < 4 && sit) =?=> { case Sit(who) =>
          seated = seated + 1; seats.add(who)
        }
          | (seated > 0 && stand) =?=> { case Stand(who) =>
            seated = seated - 1; seats.remove(who)
          })
      }
      println(s"""BUTLER DIED: ${seats.mkString(" ")} present""")
    }
  }

  // Copy messages from report onto the console
  val theConsole: PROC = proc("console") {
    repeat { Console.println(report ? ()) }
  }

  // Channels to pick up and drop the forks:
  val philToLeftFork =
    for (i <- 0 until N) yield OneOne[Action](s"Phil($i) to Fork($i)")
  val philToRightFork =
    for (i <- 0 until N)
      yield OneOne[Action](s"Phil($i) to Fork(${(N + i - 1) % N})")
  // philToLeftFork(i)  is from Phil(i) to Fork(i);
  // philToRightFork(i) is from Phil(i) to Fork((i-1)%N)

  // Put the components together
  val AllPhils: PROC = ||(
    for (i <- 0 until N) yield Phil(i, philToLeftFork(i), philToRightFork(i))
  )

  val AllForks: PROC = ||(
    for (i <- 0 until N)
      yield Fork(i, philToRightFork((i + 1) % N), philToLeftFork(i))
  )

  val System: PROC = AllPhils || AllForks || Butler(sit, stand) || theConsole

  // And run it
  def main(args: Array[String]) = {
    println(debugger)
    System()
  }
}
