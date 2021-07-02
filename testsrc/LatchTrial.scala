import io.threadcso._
import ox.app.OPT.{OPT, _}

/** Perfunctory test of a Latch
  *
  *
  * */
object LatchTrial extends App {
  var waiters      = 100
  var latch        = 100
  var decrements   = 50
  var debug        = false
  var delay        = 0l

  val Command = "LatchTrial"

  val Options = List ( OPT("-w", waiters,      "# waiting processes"),
                       OPT("-l", latch,        "# latch size"),
                       OPT("-n", decrements,   "# decrements per decrementer process "),
                       OPT("-d", debug, true,  "start the debugger"),
                       OPT("-s", delay,        "microseconds delay between decrements")
                     )

 def Main: Unit =
 { if (debug) println(debugger)
   val l = io.threadcso.semaphore.Latch(latch, "Latch")
   val done = CountingSemaphore(name="Done", available=0)
   def w(n:Int) = proc(s"w$n") { l.await; done.release }
   def d(n:Int) = proc(s"d$n") { for (i<-0 until decrements) { l.decrement; sleep(delay*microSec) }}

   val Observer = proc("Observer")
   { var i=0
     while (done.tryAcquire(seconds(5))) { i+=1; print(s"$i ") }
     println()
     println(s"Finished waiting for unlatching -- $done")
   }

   run( || (for (i<- 0 until waiters) yield w(i))
        ||
        || (for (i<- 0 until (latch / decrements)) yield d(i))
        || Observer
      )
   println(s"Terminated -- $done")

 }
}
