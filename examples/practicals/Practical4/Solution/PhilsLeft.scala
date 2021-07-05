// Dining Philosophers, with one right hander

import io.threadcso._
import scala.language.postfixOps

object PhilsLeft 
{

  // Philosophers' actions 
  abstract class Action {}
  case class Pick(who: Int)  extends Action
  case class Drop(who: Int)  extends Action

  val N = 5 // Number of philosophers

  val random = new scala.util.Random

  // Simulate basic actions
  def Eat   = sleep(50*milliSec)
  def Think = sleep(random.nextInt(80)*milliSec) 
  def Pause = sleep(50*milliSec)

  // channel to report what's happening  
  val report = N2NBuf[String] (0, 0, 1, "report")
  
  // A single philosopher: 0 is right-handed
  def Phil(me: Int, left: ![Action], right: ![Action]) = proc("Phil"+me) {
    repeat {
      Think
      if (me==0) {
         right!Pick(me); report!(s"$me picks up right fork"); Pause
         left!Pick(me);  report!(s"$me picks up left fork");  Pause
      } else {
         left!Pick(me);  report!(s"$me picks up left fork");  Pause
         right!Pick(me); report!(s"$me picks up right fork"); Pause
      }
      report ! (s"$me eats"); Eat
      left!Drop(me);  report!(s"$me drops left fork"); Pause
      right!Drop(me); report!(s"$me drops right fork"); Pause
    }
    println(s"Phil $me DIED")
  }


  // A single fork
  def Fork(me: Int, left: ?[Action], right: ?[Action]) = proc(s"Fork $me") {
    var owner: String="Nobody"
    withDebuggerFormat(s"Fork ${me} with $owner")
    {
      serve
      {(  left  =?=> { case Pick(x) => owner=s"$x"; left  ?  { case Drop(y) => assert(y==x); owner="nobody"} }
       |  right =?=> { case Pick(x) => owner=s"$x"; right ?  { case Drop(y) => assert(y==x); owner="nobody"} }
      )}
      println(s"FORK $me DIED: owned by ${owner}")
    }
  }

  // Copy messages from report onto the console
  val theConsole: PROC = proc ("Console") { repeat{ Console.println(report?()) } }

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

  val System: PROC = AllPhils || AllForks || theConsole

  // And run it
  def main(args : Array[String]) = 
  { println(debugger)
    System()
  } 
}

  











