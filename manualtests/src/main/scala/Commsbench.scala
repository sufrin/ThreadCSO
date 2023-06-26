import io.threadcso._
import scala.language.postfixOps

/** <p>Communication time benchmarks.
  *
  * Measures communication overheads in a network of the form
  *
  * {{{
  * PREFIX ---> DELTA ---> CONSUMER
  * |           |
  * +<---SUCC<--+
  * }}}
  *
  * by injecting a datum (or data) at prefix and seeing how long it takes for
  * the consumer to read a fixed number of times. The published \G is a measure
  * of the communication overhead.
  *
  * The transport (--> and <-- in the diagram) can be synchronized or
  * asynchronous. When asynchronous the radius of the buffers can be set.
  *
  * There is also an experiment that measures the overhead of spawning (null)
  * processes; this really matters when the experiment is done with a parallel
  * delta (a delta that outputs to consumer and succ in parallel).
  */
object Commsbench {
  import io.threadcso.channel._ // default ports and transport are non-alt

  println("CSO (channel based) benchmark for io.threadcso message passing")

  var debug = false
  var alting = false

  def time: Long = java.lang.System.nanoTime

  def id(in: ??[Int], out: !![Int]) =
    proc {
      repeat { out ! (in ? ()) }
      out.closeOut()
      in.closeIn()
    }

  def succ(in: ??[Int], out: !![Int]) =
    proc {
      repeat { out ! (1 + (in ? ())) }
      out.closeOut()
      in.closeIn()
    }

  def prefix(n: Int, in: ??[Int], out: !![Int]): PROC = prefix(1, n, in, out)

  def prefix(m: Int, n: Int, in: ??[Int], out: !![Int]): PROC =
    proc {
      for (i <- 0 until m) out ! n
      id(in, out)()
    }

  def delta(in: ??[Int], out1: !![Int], out2: !![Int]) =
    proc {
      var n = 0
      val out = proc { out1 ! n } || proc { out2 ! n }
      repeat {
        n = in ? ()
        out()
      }
      in.closeIn()
      out1.closeOut()
      out2.closeOut()
    }

  def seqdelta(in: ??[Int], out1: !![Int], out2: !![Int]) =
    proc {
      var n = 0
      repeat {
        n = in ? ()
        out1 ! n
        out2 ! n
      }
      in.closeIn()
      out1.closeOut()
      out2.closeOut()
    }

  def consume(caption: String, comms: Int, loops: Int, in: ??[Int]) =
    proc {
      for (i <- 0 until 2000) in ? ()
      val ts = time
      var n = loops
      while (n != 0) { in ? (); n = n - 1 }
      val te = (time - ts) / 1000.0
      in.closeIn()
      printf(
        "%-20s: %d loops; %fμ/loop; %fμ/comm%n",
        caption,
        loops,
        te / loops,
        te / (loops * comms)
      )
    }

  def comms(out: !![Int]) =
    proc {
      val a, b, c =
        if (alting) alternation.channel.OneOne[Int]() else OneOne[Int]()
      (prefix(0, b, a)
        || seqdelta(a, c, out)
        || succ(c, b))()
    }

  def experiment(
      loops: Int,
      caption: String,
      inject: Int,
      chan: => Chan[Int],
      delta: (??[Int], !![Int], !![Int]) => PROC
  ): Unit = {
    val a, b, c, d = chan
    (prefix(inject, 0, b, a)
      || delta(a, c, d)
      || succ(c, b)
      || consume(
        (if (alting) "-a " else "+a ") + inject + "-" + caption,
        4,
        loops,
        d
      ))()
  }

  def mkBuf(size: Int): Chan[Int] =
    if (alting) OneOneBuf[Int](size) else channel.OneOneBuf(size)
  def mkOne(): Chan[Int] = if (alting) OneOne[Int]() else channel.OneOne()

  def seqbuf(loops: Int, inject: Int) =
    experiment(loops, "Seq, Buf", inject, { mkBuf(2 * inject) }, seqdelta)

  def nullprocess(loops: Int, width: Int): Unit = {
    val count = loops
    val par: PROC = ||(for (i <- 0 until width) yield proc { val v = 0 })
    val ts = time
    var n = count
    while (n != 0) {
      n = n - 1; par(); if (debug) if (n % 100 == 0) Console.print("*")
    }
    val te = (time - ts) / 1000.0
    printf(
      "%-20d null processes; %fμ; %fμ/proc%n",
      count * width,
      te,
      te / (count * width * 1.0)
    )
  }

  val prompt = """One of
  -xNUMBER      number of experiments to conduct (default is 10)
  -p            Experiment is (pardelta-oneone) injecting one datum,
  -s            Experiment is (seqdelta-oneone) injecting one datum,
  -pNUMBER      Experiment is (pardelta-Buf(2*NUMBER)) injecting NUMBER data
  -sNUMBER      Experiment is (seqdelta-Buf(2*NUMBER)) injecting NUMBER data,
  -nNUMBER      Experiment is (run || NUMBER null processes)
  -PNUMBER      set threadpool expiry time to NUMBER seconds
  -NNUMBER      Set the number of iterations er experiment (default 10000)
  -a            Use alt-capable transport
  +a            Use non-alt-capable transport (default)
  -i            turn on threadpool instrumentation
  -d            desperation measure for checking par termination in an -n experiment%n"""

  def main(args: Array[String]): Unit = {
    var n = 10
    var loops = 10000
    if (args.length == 0)
      printf(prompt)
    else
      for (arg <- args)
        if (arg == "+a") alting = false
        else if (arg == "-a") alting = true
        else if (arg == "-p")
          for (i <- 0 until n)
            experiment(loops, "Par, OneOne", 1, { mkOne() }, delta)
        else if (arg == "-s")
          for (i <- 0 until n)
            experiment(loops, "Seq, OneOne", 1, { mkOne() }, seqdelta)
        else if (arg startsWith "-s") {
          val inject = arg substring 2 toInt;
          for (i <- 0 until n)
            experiment(
              loops,
              "Seq, Buf",
              inject,
              { mkBuf(2 * inject) },
              seqdelta
            )
        } else if (arg startsWith "-p") {
          val inject = arg substring 2 toInt;
          for (i <- 0 until n)
            experiment(loops, "Par, Buf", inject, { mkBuf(2 * inject) }, delta)
        } else if (arg startsWith "-n")
          for (i <- 0 until n)
            nullprocess(loops, arg substring 2 toInt)
        else if (arg startsWith "-x")
          n = (arg substring 2 toInt)
        else if (arg startsWith "-N")
          loops = (arg substring 2 toInt)
        else if (arg == "-d") {
          debug = true
          println(debugger)
        } else if (arg == "-i") {
          java.lang.System.setProperty("ox.cso.pool.instrument", "1")
        } else if (arg startsWith "-P") {
          java.lang.System
            .setProperty("io.threadcso.pool.SECS", arg substring 2)
        } else if (arg startsWith "-K") {
           val poolKind = arg.substring(2)
           scala.util.Properties.setProp("io.threadcso.pool.KIND", poolKind)
        }
        else
          printf(prompt)
    if (debug) println("Connect to the debugger now if you wish") else exit()
  }
}
