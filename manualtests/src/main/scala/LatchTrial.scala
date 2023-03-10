import io.threadcso._
import app.OPT.{OPT, _}

/**
  *  Perfunctory test of a Latch
  */
object LatchTrial extends App {
  var waiters    = 100
  var latch      = 100
  var decrements = 50
  var debug      = false
  var delay      = 0L
  var poolKind   = "VIRTUAL"

  val Command = "LatchTrial"

  val Options = List(
    OPT("-w", waiters, "# waiting processes"),
    OPT("-l", latch, "# latch size"),
    OPT("-n", decrements, "# decrements per decrementer process "),
    OPT("-d", debug, true, "start the debugger"),
    OPT("-s", delay, "microseconds delay between decrements"),
    OPT("-k", poolKind, "thread pool kind VIRTUAL ADAPTIVE")
  )

  def Main(): Unit = {
    if (debug) println(debugger)
    scala.util.Properties.setProp("io.threadcso.pool.KIND", poolKind)
    val l    = io.threadcso.semaphore.Latch(latch, "Latch")
    
    val done = CountingSemaphore(name = "Done", available = 0)

    /** A waiter process */
    def w(n: Int) = proc(s"w$n") { l.await(); done.release() }

    /** A latch decrementer process */
    def d(n: Int) = proc(s"d$n") {
      for (i <- 0 until decrements) { l.decrement(); sleep(delay * microSec) }
    }

    val Observer = proc("Observer") {
      var i = 0
      while (done.tryAcquire(seconds(1))) { i += 1; print(s"$i ") }
      println()
      println(s"Finished waiting for unlatching -- $done/$i")
    }

    // the main program 
    run(  (||(for (i <- 0 until waiters) yield w(i)))
       || (||(for (i <- 0 until (latch / decrements)) yield d(i)))
       || Observer
    )
    println(s"Terminated -- $done")

  }
}
