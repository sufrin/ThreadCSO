import io.threadcso._
import io.threadcso.component._
import app.OPT.{OPT, _}
import io.SourcePath._

import scala.language.postfixOps

/** <p> A mixin to deal with the parsing of arguments specifying parameters of
  * one or more component trials.
  * {{{
  * -t «long» ns delay (1000)
  * -T «long» ns tick delay (1000)
  * -p Run parallel trials
  * -A Use alt-capable channel
  * -a «int» take average over this number of trials
  * -d start the debugger
  * -m «int»number of messages to invent and send in Messages (100000)
  * -s «n» number of senders (20)
  * -r «n» number of receivers (20)
  * -b «n» radius of buffers (-1) [-1 means use synchronized channel; 0 means unbounded]
  * -N «n» set N (1000)
  * -tr «bits» set trace level to «bits» (default 0)
  * -log «n» set trace-log length to  (default 300)
  * -K «poolKind» set thread pool 
  * }}}
  */
// noinspection UnitMethodIsParameterless, UnitMethodIsParameterless, VarCouldBeVal, 
// VarCouldBeVal, VarCouldBeVal, VarCouldBeVal, VarCouldBeVal, VarCouldBeVal, 
// VarCouldBeVal, VarCouldBeVal, VarCouldBeVal
abstract class ComponentTrial(doc: String)(implicit loc: SourceLocation)
    extends App {
  implicit class MkString[T](s: Seq[T]) {
    def asSeq: String = s.mkString("[", ", ", "]")
  }

  val Command: String = doc

  val args = new scala.collection.mutable.Queue[String]
  var time = 1000L
  var tick = 1000L
  var debug = false
  var senders = 20
  var receivers = 20
  var bufSize: Int = -1
  var N = 1000
  var dwell = 1000L
  val MS = 1000000L
  var trials = 10
  var messages = 100000
  var sequential = true
  var nonalt = true
  var poolwait = 0
  var trace = 0
  var logsize = 300
  var poolKind = "VIRTUAL"

  // noinspection NameBooleanParameters
  val Options = List(
    OPT("-t", time, s"«long» ns delay ($time)"),
    OPT("-T", tick, s"«long» ns tick delay ($tick)"),
    OPT("-p", sequential, false, s"Run parallel trials"),
    OPT("-A", nonalt, false, s"Use alt-capable factory for Commstime"),
    OPT("-a", trials, s"«int» take average over this number of trials"),
    OPT("-d", debug, s"start the debugger"),
    OPT("-dwell", dwell, s"dwell time ($dwell) ms"),
    OPT(
      "-m",
      messages,
      s"«int» number of messages to invent and send ($messages) in Messages"
    ),
    OPT("-s", senders, s"«n» number of senders ($senders)"),
    OPT("-r", receivers, s"«n» number of receivers ($receivers)"),
    OPT(
      "-b",
      bufSize,
      s"«n» radius of buffers ($bufSize) [-1 means use synchronized channel; 0 means unbounded]"
    ),
    OPT("-N", N, s"«n» set N ($N)"),
    OPT("-W", poolwait, s"«n» set pool wait time (seconds) to «n»"),
    OPT("-tr", trace, s"«bits» set trace level to «bits» ($trace)"),
    OPT("-log", logsize, s"«n» set log radius ($logsize) to «n»"),
    OPT("-K", poolKind, s"set thread pool kind: ($poolKind) to on e of VIRTUAL UNPOOLED ADAPTIVE etc"),
    ELSE("<...>", (l => args.enqueue(l)), s"add this to the list of arguments")
  )

  def now: Long = System.nanoTime
  def readLine: String = scala.io.StdIn.readLine()

  val log = new io.threadcso.debug.Logger("Trace", logsize, trace)

  import io.SourcePath._

  def tr(event: Any)(implicit location: SourceLocation): Unit =
    log.log(1, event.toString)(location)

  def MAIN: Unit

  def Main(): Unit = {
    if (debug) {
      println(s"Runnable $loc")
      println(debugger)
    }

    scala.util.Properties.setProp("io.threadcso.pool.KIND", poolKind)
    
    if (poolwait > 0)
      java.lang.System.setProperty("io.threadcso.pool.SECS", poolwait.toString)
    val beginTime = nanoTime
    MAIN
    println(
      s"$Command ($loc) terminated: elapsed ${(nanoTime - beginTime).hms}"
    )
    if (debug) println("Inspect traces using the debugger") else exit()
  }

  def Pipe[T](name: String): Chan[T] =
    if (bufSize < 0) OneOne[T](name) else OneOneBuf(bufSize, name)
}

/** Echos keyboard lines to console */
object Echo extends ComponentTrial("Echo -- echos keyboard lines to console") {
  def MAIN: Unit = {
    var n = 0
    val kbd = OneOne[String]("kbd")
    if (debug) debugger.monitor("n", { n.toString })
    run(
      keyboard(kbd, { n += 1; s"$n> " })
        || console(kbd)
    )
  }
}

/** Echos time-stamped keyboard lines to console */
object TimeStamped
    extends ComponentTrial(
      "TimeStamped -- echos time-stamped keyboard lines to console"
    ) {
  def MAIN: Unit = {
    var n = 0
    val kbd = OneOne[String]("kbd")
    val tick = component.Timer(time, nanoTime)
    val mid = OneOne[(Nanoseconds, String)]("mid")
    val out = OneOne[String]("out")
    if (debug) debugger.monitor("n", { n.toString })
    run(
      keyboard(kbd, { n += 1; s"$n> " })
        || zip(tick, kbd, mid)
        || map[(Nanoseconds, String), String]({
          case (n: Nanoseconds, s: String) => s"$s @ ${n.hms}"
        })(mid, out)
        || console(out)
    )
  }
}

/** Echos time-stamped ticks to console */
object Ticked
    extends ComponentTrial("Ticked -- echos time-stamped ticks to console") {
  def MAIN: Unit = {
    var n = 0
    val ticker = component.Ticker(tick)
    val timer = component.Timer(time, nanoTime)
    val mid = OneOne[(Nanoseconds, Unit)]("mid")
    val out = OneOne[String]("out")
    if (debug) debugger.monitor("n", { n.toString })
    run(
      zip(timer, ticker, mid)
        || map[(Nanoseconds, Unit), String]({ case (n: Nanoseconds, _: Unit) =>
          s"@ ${n.hms}"
        })(mid, out)
        || console(out)
    )
  }
}

/** A light test of shared factory. Runs writers in parallel with readers
  * transmitting args down an N2N/N2NBuf mid
  */
object Shared extends ComponentTrial("Shared") {
  import io.SourcePath._

  def writer(me: Int, out: channel.!![String]): PROC =
    π(s"writer($me)") {
      repeatFor(args) { arg => out ! arg; tr(("W", me, arg)) }
      tr(("W", me, "done"))
      out.closeOut()
    }

  def reader(me: Int, in: channel.??[String]): PROC =
    π(s"reader($me)") {
      var go = true
      repeat(go) {
        val s = in ? ()
        Console.println(s)
        if (s == "@") sleep(seconds(5.5))
        go = s != "."
        tr(("R", me, s, go))
      }
      tr(("R", me, go, "done"))
      in.closeIn()
    }
  def MAIN: Unit = {
    val mid =
      if (bufSize < 0)
        N2N[String](senders, receivers, "mid")
      else
        N2NBuf[String](bufSize, senders, receivers, "mid")
    val writers = ||(for (i <- 0 until senders) yield writer(i, mid))
    val readers = ||(for (i <- 0 until receivers) yield reader(i, mid))
    if (debug) {
      debugger.monitor("args", args.toString)
      debugger.monitor("mid", mid.toString)
    }
    run(writers || readers)

  }
}

/** A test of shared factory. Runs writers in parallel with readers:
  * transmitting messages down an N2N/N2NBuf mid, without printing them. Active
  * readers contend for messages, active writers contend for the channel.
  */
object Messages extends ComponentTrial("Messages") {
  val n = new java.util.concurrent.atomic.AtomicInteger(senders)
  import io.SourcePath._

  /** write `messages+1` successive integers to `out` (log once every 1000) then
    * `out.closeOut`
    */
  def writer(me: Int, out: channel.!![Int]): PROC =
    π(s"writer($me)") {
      repeatFor(0 until messages) { arg =>
        out ! arg
        if (arg % 1000 == 0) tr(("W", me, arg))
      }
      out ! messages
      tr(("W", me, "DONE"))
      out.closeOut()
    }

  /** While there are readers active read messages from `in`, and log if a
    * message is a multiple of 1000. Log (and diminish the count of active
    * readers) if the message is the number `messages`.
    */
  def reader(me: Int, in: channel.??[Int]): PROC =
    π(s"reader($me)") {
      repeat(n.get() > 0) {
        val s = in ? ()
        if (s % 1000 == 0) {
          tr(("R", me, s, n))
        }
        if (s == messages) {
          tr(("R", me, s, n))
          n.decrementAndGet()
        }
      }
      tr(("R", me, n, "DONE"))
      in.closeIn()
    }

  def MAIN: Unit = {
    val mid =
      if (bufSize < 0)
        N2N[Int](senders, receivers, "mid")
      else
        N2NBuf[Int](bufSize, senders, receivers, "mid")
    val writers = ||(for (i <- 0 until senders) yield writer(i, mid))
    val readers = ||(for (i <- 0 until receivers) yield reader(i, mid))
    if (debug) {
      debugger.monitor("args", args.toString)
      debugger.monitor("mid", mid.toString)
    }
    run(writers || readers)

  }
}

/** Copies named files to the console -- interleaving lines ad-hoc if there is
  * more than one
  */
object FileCopy extends ComponentTrial("FileCopy") {
  def MAIN: Unit = {
    val term: Chan[String] =
      N2N[String](writers = args.length, readers = 1, name = "terminal")

    val readers = ||(for (arg <- args) yield component.lines(arg, term))

    (readers || component.console(term))()
  }
}

/** Runs `N` target loops repeatedly printing their identities. Responds to a
  * typed identity by interrupting it
  */
object Interrupt extends ComponentTrial("Interrupt") {
  def wasInterrupted[T](body: => T): Boolean = {
    try { body }
    catch { case _: java.lang.InterruptedException => return true }
    false
  }

  def target(me: Int): PROC = π(s"target($me)") {
    val sleep = OneOne[Unit]()
    repeat(!interrupted()) {
      if (wasInterrupted { sleep.readBefore(dwell * MS) }) {
        Console.println(s"$me cancelled while asleep")
        sleep.close()
      } else
        Console.println(me)
    }
    Console.println(s"$me cancelled")
  }

  def MAIN: Unit = {
    val targets = for (i <- 0 until N) yield target(i)
    val handles = for (t <- targets) yield fork(t)
    repeat {
      val l = readLine
      if (l == "") stop
      handles(l.toInt).interrupt()
    }
  }
}

/** Relays keyboard lines to console; closes the relay and stops when a . is
  * read.
  */
object CloseKeyboard extends ComponentTrial("CloseKeyboard") {
  def relay(in: channel.??[String], out: channel.!![String]): PROC = proc {
    repeat { in ? (l => if (l == ".") stop else out ! l) }
    in.closeIn()
    out.closeOut()
  }

  def MAIN: Unit = {
    val k, l = OneOne[String]("k")
    println(debugger)
    (keyboard(k, { "> " })
      || relay(k, l)
      || console(l))()
  }
}

/** This is a tad slower than commscso because the factory are referenced as
  * free variables.
  *
  * Measures communication overheads in a network of the form
  * {{{
  * PREFIX ---> DELTA ---> CONSUMER
  * |           |
  * +<---SUCC<--+
  * }}}
  * by injecting a datum (or data) at prefix and seeing how long it takes for
  * the consumer to read a fixed number of times. The published \G is a measure
  * of the communication overhead.
  *
  * The factory (--> and <-- in the diagram) can be synchronized or
  * asynchronous. When asynchronous the radius of the buffers can be set.
  */
object Commstime extends ComponentTrial("Commstime") {
  def MAIN: Unit = {

    def newChan(): channel.Chan[Int] =
      if (nonalt)
        if (bufSize >= 0)
          io.threadcso.channel.N2NBuf[Int](3 * bufSize, 1, 1)
        else
          io.threadcso.channel.OneOne[Int]()
      else if (bufSize >= 0)
        OneOneBuf[Int](3 * bufSize)
      else
        OneOne[Int]

    val inject =
      if (bufSize < 0) 1 else if (bufSize == 0) 4 else bufSize

    var a, b, c, d: channel.Chan[Int] = null

    val Prefix =
      proc("Prefix") {
        for (_ <- 0 until inject) a ! 0
        repeat {
          a ! (b ? ())
        }
        a.closeOut(); b.closeIn()
      }

    val Succ =
      proc("Succ") {
        repeat {
          b ! ((c ? ()) + 1)
        }
        b.closeOut(); c.closeIn()
      }

    val SeqDelta =
      proc("SeqDelta") {
        repeat {
          val n = a ? (); c ! n; d ! n
        }
        a.closeIn(); c.closeOut(); d.closeOut()
      }

    val ParDelta =
      proc("ParDelta") {
        var n = a ? ()
        val out = proc {
          c ! n
        } || proc {
          d ! n
        }
        repeat {
          out(); n = a ? ()
        }
        a.closeIn()
        c.closeOut()
        d.closeOut()
      }

    val Sink =
      proc("Sink") {
        repeat {
          val v = d ? (); if (v > N) d.closeIn()
        }
      }

    val SeqCommstime = (Prefix || SeqDelta || Succ || Sink)
    val ParCommstime = (Prefix || ParDelta || Succ || Sink)

    val Trial: PROC =
      (if (sequential) SeqCommstime else ParCommstime)

    var elapsed: Long = 0

    def trial(): Unit = {
      val now = nanoTime
      a = newChan()
      b = newChan()
      c = newChan()
      d = newChan()
      Trial()
      elapsed += nanoTime - now
      ()
    }

    val caption = if (sequential) "Seq" else "Par"
    for (_ <- 0 until trials) trial()
    println(
      s"Cold $caption: $trials trials of $N tokens (injecting $inject) took ${elapsed * 1.0}ns @ ${(elapsed * 1.0) / (1.0e3 * N * trials)}μs cycle"
    )

    elapsed = 0
    for (_ <- 0 until trials) trial()
    println(
      s"Warm $caption: $trials trials of $N tokens (injecting $inject) took ${elapsed * 1.0}ns @ ${(elapsed * 1.0) / (1.0e3 * N * trials)}μs cycle"
    )

    elapsed = 0
    for (_ <- 0 until trials) trial()
    println(
      s"Warm $caption: $trials trials of $N tokens (injecting $inject) took ${elapsed * 1.0}ns @ ${(elapsed * 1.0) / (1.0e3 * N * trials)}μs cycle"
    )

    elapsed = 0
    for (_ <- 0 until trials) trial()
    println(
      s"Warm $caption: $trials trials of $N tokens (injecting $inject) took ${elapsed * 1.0}ns @ ${(elapsed) * 1.0 / (1.0e3 * N * trials)}μs cycle"
    )

  }

  /* Benchmarks
   * AGONISTICAL JVM 1.8: after channel reimplementations
   * Cold Seq: 100 trials of 1000 tokens (injecting 1) took 3.488429151E9ns @ 34.88429151μs cycle
   * Warm Seq: 100 trials of 1000 tokens (injecting 1) took 3.434888064E9ns @ 34.34888064μs cycle
   * Warm Seq: 100 trials of 1000 tokens (injecting 1) took 3.446285206E9ns @ 34.46285206μs cycle
   *
      **************************************** all these measured before channel reimplementation *******************
   *xso -Dio.threadcso.pool=10 test.Commstime -a 100 -N 1000
      **
   *BOUNCER JVM1.7
   * Cold Seq: 100 trials of 1000 tokens took 4.74960076E9ns @ 47.4960076μs cycle
   * Warm Seq: 100 trials of 1000 tokens took 3.914447058E9ns @ 39.14447058μs cycle
   * Warm Seq: 100 trials of 1000 tokens took 4.085051323E9ns @ 40.85051323μs cycle
      **
   *MIMI JVM 1.7 and 1.8
   * Cold Seq: 100 trials of 1000 tokens took 3.085613E9ns @ 30.85613μs cycle
   * Warm Seq: 100 trials of 1000 tokens took 2.925029E9ns @ 29.25029μs cycle
   * Warm Seq: 100 trials of 1000 tokens took 2.924609E9ns @ 29.24609μs cycle
      **
   *AGONISTES JVM1.7 (JVM 1.8 within 2% of this)
   * Cold Seq: 100 trials of 1000 tokens took 5.568185E9ns @ 55.68185μs cycle
   * Warm Seq: 100 trials of 1000 tokens took 5.325333E9ns @ 53.25333μs cycle
   * Warm Seq: 100 trials of 1000 tokens took 5.256115E9ns @ 52.56115μs cycle
      **
   *AGONISTES PREVIOUS BENCHMARK (from the 2008 paper)
   * 31-34 μs per cycle
      **
   *  Disappointing results after subsequent development of alting, etc.
   *  Explanation: the earlier implemention was incorrect wrt. synchronization
      **
   *MIMI JVM 1.7 and 1.8 with -b 5
   * Cold Seq: 100 trials of 1000 tokens (injecting 5) took 2.1175E9ns @ 21.175μs cycle
   * Warm Seq: 100 trials of 1000 tokens (injecting 5) took 1.993625E9ns @ 19.93625μs cycle
   * Warm Seq: 100 trials of 1000 tokens (injecting 5) took 1.988099E9ns @ 19.88099μs cycle
      **
   *DITTO unbuffered
   * Cold Seq: 100 trials of 1000 tokens (injecting 1) took 5.684899E9ns @ 56.84899μs cycle
   * Warm Seq: 100 trials of 1000 tokens (injecting 1) took 5.513198E9ns @ 55.13198μs cycle
   * Warm Seq: 100 trials of 1000 tokens (injecting 1) took 5.499643E9ns @ 54.99643μs cycle
   *
   *
   */

}
