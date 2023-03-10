// Dining Philosophers with success/fail protocol to forks

/** It is noticeable how infrequently it turns out to be necessary for the first
  * fork to be put down because the second fork isn't forthcoming. The backoff
  * vs. eating-time ratio is (probably) a significant factor in this. Backing
  * off if the first fork isn't available improved the retry frequency aspect of
  * the performance of the simulation below, though I haven't measured
  * ''throughput''.
  */

import io.threadcso._
import scala.language.postfixOps

object PhilsProto {

  val N = 5 // Number of philosophers

  abstract class Action {}
  case class Pick(who: Int) extends Action
  case class Drop(who: Int) extends Action
  case class Sit(who: Int)
  case class Stand(who: Int)

  /** A class that provides communication from a client to a service and back */
  class Conn[T, U](name: String) {
    val fromClient = OneOne[T](name)
    val toClient = OneOne[U](s"reply-$name")
    def !?(t: T): U = { fromClient ! t; toClient ? () }
    def !(t: T): Unit = { fromClient ! t }
  }

  // channel to report what's happening
  val report = N2NBuf[String](20, 0, 1, "report")

  // Protocol
  type ForkChan = Conn[Action, Boolean]
  def forkChan(name: String) = new Conn[Action, Boolean](name)

  // A single philosopher
  def Phil(me: Int, left: ForkChan, right: ForkChan) = proc("Phil" + me) {
    // Each philosopher has its own random generator
    val random = new scala.util.Random
    // Simulate basic actions
    @inline def Eat = sleep((25 + random.nextInt(50)) * milliSec)
    @inline def Think = sleep(random.nextInt(80) * milliSec)
    @inline def Pause = sleep((50 + random.nextInt(25)) * milliSec)
    @inline def Backoff = sleep(75 + random.nextInt(80) * milliSec)
    val lf = me
    val rf = (N + me - 1) % N

    repeat {
      Think
      var gotBoth = false
      while (!gotBoth) {
        if (left !? Pick(me)) {
          report ! (s"$me picks up left fork $lf")
          Pause
          if (right !? Pick(me)) {
            gotBoth = true
            report ! (s"$me picks up right fork $rf")
          } else {
            report ! (s"$me ** replacing left fork $lf and waiting")
            left ! Drop(me)
            Backoff
            report ! (s"$me ** starting again")
          }
        } else
          Backoff
      }
      report ! (s"$me ready to eat"); Pause
      report ! (s"$me eats"); Eat
      report ! (s"$me dropping left fork $lf"); left ! Drop(me);
      report ! (s"$me dropped left fork $lf"); Pause
      report ! (s"$me dropping right fork $rf"); right ! Drop(me);
      report ! (s"$me drops right fork $rf"); Pause
    }
    println(s"Phil $me DIED")
  }

  object Owned extends Enumeration { val LEFT, RIGHT, NOBODY = Value }

  // A single fork with an atomic conditional pick-up request: anyone can ask, but they may be refused
  def Fork(me: Int, left: ForkChan, right: ForkChan) = proc("Fork" + me) {
    var owner = -1
    var owned = Owned.NOBODY

    withDebuggerFormat(s"Fork ${me} with $owner/$owned") {
      serve {
        (left.fromClient =?=> {
          case Pick(x) =>
            if (owned == Owned.NOBODY) {
              owner = x
              owned = Owned.LEFT
              left.toClient ! true
            } else
              left.toClient ! false

          case Drop(y) =>
            if (owned == Owned.LEFT && y == owner) {
              owner = -1
              owned = Owned.NOBODY
            } else
              throw new Error(
                s"fork $me (owned by $owner/$owned) receives Drop($y)"
              )
        }
          | right.fromClient =?=> {
            case Pick(x) =>
              if (owned == Owned.NOBODY) {
                owner = x
                owned = Owned.RIGHT
                right.toClient ! true
              } else
                right.toClient ! false

            case Drop(y) =>
              if (owned == Owned.RIGHT && y == owner) {
                owner = -1
                owned = Owned.NOBODY
              } else
                throw new Error(
                  s"fork $me (owned by $owner/$owned) receives Drop($y)"
                )
          })
      }
      println(s"FORK $me DIED: owned by ${owner}")
    }
  }

  // Copy messages from report onto the console
  val theConsole: PROC = proc("console") {
    repeat { Console.println(report ? ()) }
  }

  // Channels to pick up and drop the forks:
  val philToLeftFork =
    for (i <- 0 until N) yield forkChan(s"Phil($i) to Fork($i)")
  val philToRightFork =
    for (i <- 0 until N) yield forkChan(s"Phil($i) to Fork(${(N + i - 1) % N})")
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

  val System: PROC = AllPhils || AllForks || theConsole

  // And run it
  def main(args: Array[String]) = {
    println(debugger)
    System()
  }
}
