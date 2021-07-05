// Dining Philosophers -- deadlocking version

import io.threadcso._
import scala.language.postfixOps


object Phils
{
  // Philosophers' actions 
  abstract class Action {}
  case class Pick(who: Int)  extends Action
  case class Drop(who: Int)  extends Action

  val N = 5 // Number of philosophers

  val random = new scala.util.Random

  // Simulate basic actions
  def Eat   = sleep(500*milliSec)
  def Think = sleep((200+random.nextInt(300))*milliSec) 
  def Pause = sleep((200+random.nextInt(300))*milliSec)
 
  val report = N2NBuf[String](20, 0, 1, "report")

  // A single philosopher
  def Phil(me: Int, left: ![Action], right: ![Action]) = proc("Phil"+me) {
    repeat {
      report!(s"$me sits")
      Think
      left!Pick(me);  report!(s"$me picks up left fork");  Pause
      right!Pick(me); report!(s"$me picks up right fork"); Pause
      report ! (s"$me eats"); Eat
      left!Drop(me);  report!(s"$me drops left fork"); Pause
      right!Drop(me); report!(s"$me drops right fork"); Pause
      report!(s"$me gets up"); Pause 
    }
  }


  // A single fork
  def Fork(me: Int, left: ?[Action], right: ?[Action]) = proc("Fork"+me) {
    var owner: String="Nobody"
    val state = new Debuggable
    {   override def toString = s"Fork ${me} is with philosopher $owner" 
        register()
    }
    serve
    {(
        left  =?=> { case Pick(x) => owner=s"$x"; left  ? { case Drop(y) => assert(y==x); owner="nobody"} }
     |  right =?=> { case Pick(x) => owner=s"$x"; right ? { case Drop(y) => assert(y==x); owner="nobody"} }
    )}
    println(s"FORK $me DIED: ${state}")
  }

  // Channels to pick up and drop the forks:
  val philToLeftFork  = 
      for (i<-0 until N) yield 
          OneOne[Action]  (s"Phil($i) to Fork($i)")
  val philToRightFork = 
      for (i<-0 until N) yield 
          OneOne[Action]  (s"Phil($i) to Fork(${(N+i-1)%N})")
  // philToLeftFork(i)  is from Phil(i) to Fork(i);
  // philToRightFork(i) is from Phil(i) to Fork((i-1)%N)


  // Put the components together
  val AllPhils: PROC = || ( 
    for (i <- 0 until N) yield 
      Phil( i, philToLeftFork(i), philToRightFork(i) ) 
  )

  val AllForks: PROC = || ( 
    for (i <- 0 until N) yield 
      Fork( i, philToRightFork((i+1)%N), philToLeftFork(i) ) 
  )

  val System: PROC = AllPhils || AllForks || component.console(report)

  // And run it
  def main(args : Array[String]) = 
  { println(debugger)
    System()
  } 
}

  






