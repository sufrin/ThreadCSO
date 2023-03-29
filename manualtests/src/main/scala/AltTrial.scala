import io.SourceLocation._
import io.threadcso._

/** <p> A mixin to deal with common argument-parsing matters that arise in
  * testing one or another aspect of the alternation implementation.
  *
  * Arguments that match one (or more) of the following may modify the set-up of
  * a test. See the tests themselves for detail.
  * {{{
  * -d    (debugger)
  * -b=0  (buffer radius -- 0 means use a OneOne)
  * -a=0  (count of arguments to synthesise)
  * -mn=4 (alternation branching factor)
  * -wp=0 (microSleep for writing)
  * -wa=0 (microSleep random base for channel a)
  * -wb=0 (microSleep random base for channel b)
  * -ws=0 (microseconds after in serve/alt -- 0 means no after)
  * -cb (close the b channel before starting -- where appropriate)
  * -ca (close the a channel before starting -- where appropriate)
  * -wr=0 (random range for microSleep: 0 prevents microSleep)
  * -Kpoolkind (set the thread pool/executor kind)
  * }}}
  * </p>
  */

abstract class AltTrial(implicit loc: SourceLocation) {

  /** Read a line from stdin */
  def readLine: String = scala.io.StdIn.readLine()

  /** Whether the debugger is to be started (and alternations are to be
    * monitored)
    */
  var useDebugger = false

  /** Size of any one-off buffers used in tests: 0 means a OneOne is used */
  var bufferSize = 0

  /** Wait time in microseconds in Alt1 producer, a reader, b reader, server
    * loop
    */
  var wp, wa, wb, ws = 0L
  var mn = 4
  var closeb, closea = false
  var nargs = 0
  var closeOnWrite = true
  var poolKind = "(default)"
  
  /* Random range of a- b-waits */
  var wr = 0

  /** The body of the main program: to be defined by an inheriting object */
  def MAIN(args: Seq[String]): Unit

  /** The entry point to every AltTrial program. Parses command-line arguments
    * (discarding those starting with `-`)
    * {{{
    * val arguments: Seq[String] = for (arg<-args; if (!arg.startsWith("-"))) yield arg
    * }}}
    * before running `main(arguments)`.
    *
    * Starts the debugger in the prologue if the `-d` switch is given on the
    * command-line; and exits the program after running `MAIN(args:
    * Seq[String])` if the `-d` switch is not given on the command-line.
    *
    * Uses the default executor pool kind unless -Kpoolkind is given
    */
  def main(args: Array[String]): Unit = {
    var arguments: Seq[String] =
      for (arg <- args.toIndexedSeq; if (!arg.startsWith("-"))) yield arg
    for (arg <- args)
      if (arg.startsWith("-K")) {
           poolKind = arg.substring(2)
           scala.util.Properties.setProp("io.threadcso.pool.KIND", poolKind)
      }  
      else if (arg.matches("-d")) useDebugger = true
      else if (arg.matches("-b=[0-9]+")) bufferSize = arg.substring(3).toInt
      else if (arg.matches("-wp=[0-9]+")) wp = arg.substring(4).toLong
      else if (arg.matches("-wa=[0-9]+")) wa = arg.substring(4).toLong
      else if (arg.matches("-wb=[0-9]+")) wb = arg.substring(4).toLong
      else if (arg.matches("-ws=[0-9]+")) ws = arg.substring(4).toLong
      else if (arg.matches("-wr=[0-9]+")) wr = arg.substring(4).toInt
      else if (arg.matches("-mn=[0-9]+")) mn = arg.substring(4).toInt
      else if (arg.matches("-ca")) closea = true
      else if (arg.matches("-ca")) closea = true
      else if (arg.matches("-cr")) closeOnWrite = false
      else if (arg.matches("-a=[0-9]+")) {
        nargs = arg.substring(3).toInt
        arguments = (for (i <- 0 until nargs) yield s"arg-$i") ++ arguments
      } else if (arg.startsWith("-")) {
        println(s"""Arguments must match one (or more) of:
            -Kpoolkind (set the default executor)
            -d (debugger)
            -b=$bufferSize (buffer radius)
            -a=$nargs (count of arguments to synthesise)
            -mn=$mn (alternation branching factor)
            -wp=$wp (microSleep for writing)
            -wa=$wa (microSleep random base for channel a)
            -wb=$wb (microSleep random base for channel b)
            -ws=$ws (microseconds after in serve/alt -- 0 means no after)
            -cb (close the b channel before starting -- where appropriate)
            -ca (close the a channel before starting -- where appropriate)
            -cr (close reply channel in Quorum at read end)
            -wr=$wr (random range for microSleep: 0 prevents microSleep)""");
        exit()
      }

    if (useDebugger) println(debugger)
    println(s"-K$poolKind -b=$bufferSize -mn=$mn -wp=$wp -wa=$wa -wb=$wb -ws=$ws -wr=$wr")
    // println(s"arguments: $arguments")
    MAIN(arguments)
    terminate()
  }

  val random = new scala.util.Random
  @inline def milliSleep(range: Int, base: Long = 0) =
    if (range > 0) {
      sleep((base + random.nextInt(range)) * milliSec)
    }

  @inline def microSleep(range: Int, base: Long = 0) =
    if (range > 0) {
      io.threadcso.debug.Logging.log(3, s"(")
      sleep((base + random.nextInt(range)) * microSec)
      io.threadcso.debug.Logging.log(3, s")")
    }

  def mkBuf[T](name: String): Chan[T] =
    if (bufferSize == 0) OneOne[T](name) else OneOneBuf[T](bufferSize, name)

  def terminate() = {
    print(s"$loc terminated: ")
    if (useDebugger)
      println("connect via the debugger to see logs (if appropriate)")
    else exit()
  }
}

/** Assembling quorums for course sessions. An example of a server providing the
  * implementation of an abstract machine.
  *
  * This tickled a (timing) problem with closing a once-only result-channel as
  * it was being read.
  *
  * It is connected with `serve` only insofar as a `Closed` exception can be
  * inherited by an enclosing serve.
  */
object AltQuorum extends AltTrial() {
  case class  Arrive(id: Int, reply: !![List[Int]])

  val sa =
    N2NBuf[Arrive](20, writers = 0, readers = 1, "sa") // ManyOne is essential
  val pa =
    N2NBuf[Arrive](20, writers = 0, readers = 1, "pa") // ManyOne is essential
  val ta =
    N2NBuf[Arrive](20, writers = 0, readers = 1, "ta") // ManyOne is essential
  val leave =
    N2NBuf[Int](20, writers = 0, readers = 1, "leave") // ManyOne is essential
  val quit = OneOne[Unit]("quit")

  /** Student arrives */
  def S(id: Int) = {
    val reply = OneOne[List[Int]](s"S-$id-reply")
    sa ! Arrive(id, reply)
    val v = reply ? (); if (!closeOnWrite) reply.closeIn(); v
  }

  /** Prof arrives */
  def P(id: Int) = {
    val reply = OneOne[List[Int]](s"P-$id-reply")
    pa ! Arrive(id, reply)
    val v = reply ? (); if (!closeOnWrite) reply.closeIn(); v
  }

  /** TA arrives */
  def T(id: Int) = {
    val reply = OneOne[List[Int]](s"T-$id-reply")
    ta ! Arrive(id, reply)
    val v = reply ? (); if (!closeOnWrite) reply.closeIn(); v
  }

  /** Participant leaves */
  def Leave(id: Int) = leave ! id

  /** Stop the server */
  def Quit() = quit ! (())

  // A server with quorum of `s, p, t` students, profs, ta.
  def server(s: Int = 1, p: Int = 1, t: Int = 1) = proc("server") {
    import scala.collection.mutable._
    val present = new HashSet[Int]
    var started = false
    val swaiting, pwaiting, twaiting = new Queue[Arrive]
    val replies = new HashSet[!![List[Int]]]

    def admit(n: Int, queue: Queue[Arrive]): Unit = {
      for (i <- 0 until n) {
        val Arrive(id, reply) = queue.dequeue()
        replies += reply
        present += id
      }
    }

    def admitIfPossible: Unit = {
      try {
        if (
          !started &&
          swaiting.size >= s &&
          pwaiting.size >= p &&
          twaiting.size >= t
        ) {
          replies.clear();
          admit(s, swaiting)
          admit(t, twaiting)
          admit(p, pwaiting)
          val quorum = present.toList
          println(s"Class will consist of $quorum")
          for (reply <- replies) {
            reply ! quorum; if (closeOnWrite) reply.closeOut()
          }
          started = true
        }
      } catch {
        case exn: Throwable => {
          println(s"Exception in admitIfPossible: $exn")
          exn.printStackTrace()
          throw exn
        }
      }
    }

    withDebuggerFormat(s"S $swaiting, T $twaiting, P$pwaiting") {
      serve(
        (started && leave) =?=> { id =>
          present -= id
          println(s"$id leaves")
          if (present.isEmpty) {
            started = false
            admitIfPossible
          }
        }
          | sa   =?=> { msg => swaiting.enqueue(msg); admitIfPossible }
          | ta   =?=> { msg => twaiting.enqueue(msg); admitIfPossible }
          | pa   =?=> { msg => pwaiting.enqueue(msg); admitIfPossible }
          | (quit) =?=> { _ => stop }
      )

      // Here when the server stops
      println(
        s"Server stopped:\n REP $replies\n S $swaiting\n T " +
          s"$twaiting\n P $pwaiting\n $sa\n $ta\n $pa\n $leave\n " +
          s"started: $started"
      )
    }
  }

  def s(id: Int) = fork(proc(s"S($id)") {
    println((id, S(id))); sleep(seconds(1)); leave ! id
  })
  def t(id: Int) = fork(proc(s"T($id)") {
    println((id, T(id))); sleep(seconds(1)); leave ! id
  })
  def p(id: Int) = fork(proc(s"P($id)") {
    println((id, P(id))); sleep(seconds(1)); leave ! id
  })
  def l(id: Int) = fork(proc { leave ! id })

  /**  This script stops after 10s */
  val script1 = proc("script1") {
    for { i <- 1 to 12 } s(100+i); // a dozen students
    t(20); t(21); t(22); t(23)
    p(10); p(11); p(12)
    sleep(10*Sec); Quit()
  }

  def MAIN(args: Seq[String]): Unit = {
    println(debugger)
    (server(2, 1, 1)
      || script1)()
  }

}

/** <p>Copies arguments to the console via a OneOne and a `serve` loop. Tests
  * `orelse` logic. Expected output: one line per argument then SENT, then ELSE,
  * then DONE.</p>
  */
object Alt0 extends AltTrial() {
  val a = OneOne[String]("a")
  def MAIN(args: Seq[String]): Unit = {
    (π { for (arg <- args) { a ! arg }; println("SENT"); a.closeOut() }
      || π {
        serve(
          useDebugger,
          a =?=> { x => println(x) }
            | orelse ==> { println("ELSE") }
        )
        println("DONE")
      })()
  }

}

/** <p>Copies arguments to the console via a OneOne and a `priserve` loop.
  * Expected output: one line per argument then SENT, then DONE.</p>
  */
object Alt0P extends AltTrial() {
  val a = OneOne[String]("a")
  def MAIN(args: Seq[String]): Unit =  
    run (  π { for (arg <- args) { a ! arg }; println("SENT"); a.closeOut() }
        || π { priserve(useDebugger, a =?=> { x => println(x) }); println("DONE") })
}

/** <p>Copies arguments to the console via a `OneOneBuf(bufferSize)` and a
  * `serve` loop. Expected output: one line per all but the last argument then
  * SENT, then the last argument then DONE
  */
object Alt0B extends AltTrial() {
  val a = mkBuf[String]("a")
  def MAIN(args: Seq[String]): Unit = {
    (  π { for (arg <- args) { a ! arg }; println("SENT"); a.closeOut() }
    || π { serve(useDebugger, a =?=> { x => println(x); microSleep(wr, wa) });
           println("DONE")}
    )()
  }
}

/** <p>Copies arguments to the console via a pair of `OneOneBuf(bufferSize)` and
  * a `serve` loop. that alternately reads from them. Expected output: an
  * interleaving of two copies of the arguments, a-SENT, b-SENT, terminated by
  * DONE.
  */
object Alt0BS extends AltTrial {
  val a = mkBuf[String]("a")
  val b = mkBuf[String]("b")
  def MAIN(args: Seq[String]): Unit = {
    (π { for (arg <- args) { a ! arg }; println("a-SENT"); a.closeOut() }
      || π { for (arg <- args) { b ! arg }; println("b-SENT"); b.closeOut() }
      || π {
        serve(
          useDebugger,
          a =?=> { x => println(s"a ! $x") }
            | b =?=> { x => println(s"b ! $x") }
        )
        println("DONE")
      })()
  }
}

/** <p> Copies arguments to the console via a pair of `OneOneBuf(bufferSize)`
  * and a `serve` loop. that alternately reads from them. Expected output: an
  * interleaving of two copies of the arguments, a-SENT, b-SENT, terminated by
  * DONE, with those routed via `a` coming first because the serve is
  * prioritised.
  */
object Alt0BSP extends AltTrial {
  val a = mkBuf[String]("a")
  val b = mkBuf[String]("b")
  def MAIN(args: Seq[String]): Unit = {
    (π { for (arg <- args) { a ! arg }; println("a-SENT"); a.closeOut() }
      || π { for (arg <- args) { b ! arg }; println("b-SENT"); b.closeOut() }
      || π {
        priserve(
          useDebugger,
          a =?=> { x => println(s"a ! $x") }
            | b =?=> { x => println(s"b ! $x") }
        )
        println("DONE")
      })()
  }
}

/** <p> Copies arguments to the console via an `N2N(1,1)` and a `priserve` loop.
  * Expected output: one line per argument then SENT, then DONE
  */
object Alt0N extends AltTrial {
  val a = N2N[String](1, 1, "a")
  def MAIN(args: Seq[String]): Unit = {
    (π { for (arg <- args) { a ! arg }; println("SENT"); a.closeOut() }
      || π {
        priserve(a =?=> println)
        println("DONE")
      })()
  }
}

/** <p>Copies each argument via an intermediate channel (or buffer) read by a
  * serve loop to two channel (a, b), from which they are printed.
  *
  * {{{
  * (  π ("a") { repeat { println(s"a ! \${a?()}"); microSleep(wr, wa)  } }
  * || π ("b") { repeat { println(s"b ! \${b?()}"); microSleep(wr, wb)  } }
  * || π ("writer") { for (arg<-args) { c!arg; sleep(wp*microSec)} ; c.closeOut() }
  * || π ("server")
  * { var arg  = a.nothing
  * var full = false
  * serve( (full && a)          =!=> { arg } ==> { full = false }
  * | (full && b)          =!=> { arg } ==> { full = false }
  * | (!full && c)         =?=> { v => arg = v; full = true }
  * | after(ws * microSec) ==>  { Console.print("*") }
  * )
  * a.closeOut()
  * b.closeOut()
  * }
  * )
  * }}}
  *
  * The serve loop simulates a 1-place buffer; alternately executing an output-
  * and an input- event.
  *
  * The round-robin policy for choosing between ready events acts in such a way
  * that only one of the output channel is ever used unless there is a largeish
  * difference between -wa and -wb (the minimum delay in microsecs after the
  * specified output channel is read).
  */
object Alt1 extends AltTrial {
  val a = OneOne[String]("a")
  val b = OneOne[String]("b")
  val c: Chan[String] = mkBuf[String]("c")
  def MAIN(args: Seq[String]): Unit = 
    (  π("a") { repeat { a ? { x => println(s"a ! ${x}"); microSleep(wr, wa) } } }
    || π("b") {
         repeat { b ? { x => println(s"b ! ${x}"); microSleep(wr, wb) } }
       }
    || π("writer") {
           for (arg <- args) { c ! arg; sleep(wp * microSec) }; c.closeOut()
       }
    || π("server") {
        var arg = a.nothing
        var full = false
        withDebuggerFormat(s"SERVER full: $full ($arg)") {
          serve(
            useDebugger,
            (full && a) =!=> { arg } ==> { full = false }
              | (full && b) =!=> { arg } ==> { full = false }
              | (!full && c) =?=> { v => arg = v; full = true }
              | after(ws * microSec) ==> { Console.print("*") }
          )
          a.closeOut()
          b.closeOut()
        }
      })()
}

/** Same as Alt1 except that the serve loop is replaced by a repeated alt.
  */
object Alt1A extends AltTrial {
  val a = OneOne[String]("a")
  val b = OneOne[String]("b")
  val c: Chan[String] = mkBuf[String]("c")

  def MAIN(args: Seq[String]): Unit = {
    (π("a") { repeat { println(s"a ! ${a ? ()}"); microSleep(wr, wa) } }
      || π("b") { repeat { println(s"b ! ${b ? ()}"); microSleep(wr, wb) } }
      || π("writer") {
        for (arg <- args) { c ! arg; sleep(wp * microSec) }; c.closeOut()
      }
      || π("server") {
        var arg = a.nothing
        var full = false
        withDebuggerFormat(s"SERVER full: $full ($arg)") {
          repeat {
            alt(
              useDebugger,
              (full && a) =!=> { arg } ==> { full = false }
                | (full && b) =!=> { arg } ==> { full = false }
                | (!full && c) =?=> { v => arg = v; full = true }
                | after(ws * microSec) ==> { Console.print("*") }
            )
          }
          a.closeOut()
          b.closeOut()
        }
      })()
  }

}

/** <p> Same as Alt1 except that the channel `c` is replaced by a collection of
  * `mn` channel, and the single outport event in the serve loop replaced by a
  * corresponding collection.
  * {{{
  * (  π ("a") { repeat { println(s"a ! \${a?()}"); microSleep(wr, wa)  } }
  * || π ("b") { repeat { println(s"b ! \${b?()}"); microSleep(wr, wb)  } }
  * || π ("writer")
  * { for (c <- chans)
  * { for (arg<-args) { c!s"\${c.name}.\$arg"; sleep(wp*microSec) } ;  c.closeOut()  }}
  * || π ("server")
  * { var arg  = a.nothing
  * var full = false
  * serve ( (full && b.outPort) =!=> { full = false; arg }
  * | (full && a.outPort) =!=> { full = false; arg }
  * | |(for (c <- chans) yield (!full && c.inPort) =?=> { v => arg = v; full = true })
  * | after(ws * microSec) ==> { Console.print("*") }
  * )
  * a.closeOut()
  * b.closeOut()
  * }
  * )
  * }}}
  */
object AltM extends AltTrial {
  val a = OneOne[String]("a")
  val b = OneOne[String]("b")
  def MAIN(args: Seq[String]): Unit = {
    val chans: Seq[Chan[String]] =
      for (i <- 0 until mn) yield mkBuf[String](s"c$i")
    if (closeb) b.close()
    if (closea) a.close()
    (π("a") { repeat { println(s"a ! ${a ? ()}"); microSleep(wr, wa) } }
      || π("b") { repeat { println(s"b ! ${b ? ()}"); microSleep(wr, wb) } }
      || π("writer") {
        for (c <- chans) {
          for (arg <- args) { c ! s"${c.name}.$arg"; sleep(wp * microSec) };
          c.closeOut()
        }
      }
      || π("server") {
        var arg = a.nothing
        var full = false
        withDebuggerFormat(s"SERVER full: $full ($arg)") {

          {
            serve(
              useDebugger,
              (full && b.outPort) =!=> { full = false; arg }
                | (full && a.outPort) =!=> { full = false; arg }
                | |(for (c <- chans) yield (!full && c.inPort) =?=> { v =>
                  arg = v; full = true
                })
                | after(ws * microSec) ==> { Console.print("*") }
            )
          }
        }
        a.closeOut()
        b.closeOut()
      })()
  }

}

/** Same as `AltM` except that the writing to the `c` channel is done
  * concurrently not sequentially.
  */
object AltMP extends AltTrial {
  val a = OneOne[String]("a")
  val b = OneOne[String]("b")
  def MAIN(args: Seq[String]): Unit = {
    val chans: Seq[Chan[String]] =
      for (i <- 0 until mn) yield mkBuf[String](s"c$i")
    if (closeb) b.close()
    if (closea) a.close()
    (π("a") { repeat { println(s"a ! ${a ? ()}"); microSleep(wr, wa) } }
      || π("b") { repeat { println(s"b ! ${b ? ()}"); microSleep(wr, wb) } }
      || ||(for (c <- chans) yield (proc(s"writer-$c") {
        for (arg <- args) { c ! s"${c.name}.$arg"; sleep(wp * microSec) };
        c.closeOut()
      }))
      || π("server") {
        var arg = a.nothing
        var full = false
        withDebuggerFormat(s"SERVER full: $full ($arg)") {

          {
            serve(
              useDebugger,
              (full && b.outPort) =!=> { full = false; arg }
                | (full && a.outPort) =!=> { full = false; arg }
                | |(for (c <- chans) yield (!full && c.inPort) =?=> { v =>
                  arg = v; full = true
                })
                | after(ws * microSec) ==> { Console.print("*") }
            )
          }
        }
        a.closeOut()
        b.closeOut()
      })()
  }

}

/** <p>A simple test of mixed input and output guards.
  *
  * Proc C` forwards each argument via channel `c` to the serve loop, then
  * closes `c`. The serve loop alternately reads a datum from `c` and forwards
  * it via whichever of `a` and `b` is ready.
  */
object Alt4 extends AltTrial {
  val a = OneOne[String]("a")
  val b = OneOne[String]("b")
  val c: Chan[String] =
    if (bufferSize == 0) OneOne[String]("c")
    else OneOneBuf[String](bufferSize, "c")
  def MAIN(args: Seq[String]): Unit = {

    (π("A") { repeat { println(s"a ! ${a ? ()}"); microSleep(wr, wa) } }
      || π("B") { repeat { println(s"b ! ${b ? ()}"); microSleep(wr, wb) } }
      || π("C") { for (arg <- args) c ! arg; c.close() }
      || π("Server") {
        var arg = a.nothing
        var full = false
        serve((full && a) =!=> { arg } ==> { full = false }
          | (full && b) =!=> { arg } ==> { full = false }
          | (!full && c) =?=> { v => arg = v; full = true })
        println("Finished")
        a.closeOut()
        b.closeOut()
      })()
  }

}

/** <p>A test of output guards. Forwards each argument via three routes:
  * {{{
  * C3: directly to `b`
  * C2: via `c` to the serve loop, thence either to `b` or to `a` depending which is ready
  * C1: via `c` to the serve loop, thence either to `b` or to `a` depending which is ready
  * }}}
  * The `CLOSER` process listens for three syncs on `close`, then closes the
  * data channel `a`, `b`, `c`; so the program should terminate.
  */
object Alt4m extends AltTrial {
  def ManyOne[T](n: String) = N2N[T](0, 1, n)
  val a = ManyOne[String]("a")
  val b = ManyOne[String]("b")
  val c = ManyOne[String]("c")
  val close = ManyOne[Unit]("close")
  def MAIN(args: Seq[String]): Unit = {

    (π("A") { repeat { println(s"a ! ${a ? ()}"); microSleep(wr, wa) } }
      || π("B") { repeat { println(s"b ! ${b ? ()}"); microSleep(wr, wb) } }
      || π("C1") { for (arg <- args) c ! s"C1:${arg}"; close ! (()) }
      || π("C2") { for (arg <- args) c ! s"C2:${arg}"; close ! (()) }
      || π("C3") { for (arg <- args) b ! s"C3:${arg}"; close ! (()) }
      || π("CLOSER") {
        for (i <- 0 until 3) close ? (); a.close(); b.close(); c.close()
      }
      || π("Server") {
        var arg = a.nothing
        var full = false
        serve((full && a) =!=> { arg } ==> { full = false }
          | (!full && c) =?=> { v => arg = v; full = true }
          | (full && b) =!=> { arg } ==> { full = false })
        println("Finished")
      })()
  }

}

/** As Alt4m but uses buffer */
object Alt4mb extends AltTrial {
  def ManyOne[T](n: String) = N2NBuf[T](bufferSize, 1, 1, name = n)
  val a = ManyOne[String]("a")
  val b = ManyOne[String]("b")
  val c = ManyOne[String]("c")
  val close = ManyOne[Unit]("close")
  def MAIN(args: Seq[String]): Unit = {

    (π("A") { repeat { println(s"a ! ${a ? ()}"); microSleep(wr, wa) } }
      || π("B") { repeat { println(s"b ! ${b ? ()}"); microSleep(wr, wb) } }
      || π("C1") { for (arg <- args) c ! s"C1:${arg}"; close ! (()) }
      || π("C2") { for (arg <- args) c ! s"C2:${arg}"; close ! (()) }
      || π("C3") { for (arg <- args) b ! s"C3:${arg}"; close ! (()) }
      || π("CLOSER") {
        for (i <- 0 until 3) close ? (); a.close(); b.close(); c.close()
      }
      || π("Server") {
        var arg = a.nothing
        var full = false
        serve((full && a) =!=> { arg } ==> { full = false }
          | (!full && c) =?=> { v => arg = v; full = true }
          | (full && b) =!=> { arg } ==> { full = false })
        println("Finished")
      })()
  }

}

/** <p>Tests the dynamic check for channel participating in more than one
  * alternation simultaneously. Should very quickly throw:
  * {{{
  * jdk.lang.IllegalStateException: Channel cannot participate in more than one alt simultaneously
  * }}}
  */
object Alt4f extends AltTrial {
  val a = N2N[String](writers = 1, readers = 1, "a")
  def MAIN(args: Seq[String]): Unit = {
    println(
      "This should throw an IllegalStateException ... from the alt implementation"
    )
    (π { serve(a =?=> { v => println(v) }) }
      || π { serve(a =!=> { "_x" }) })()
  }
}

/** <p>Forwards typed lines via forwarding channel `a` to a console-driving
  * serve loop. Closes a when . is typed, but continues to forward. The serve
  * loop prompts every three seconds, and will terminate when `a` is closed.
  */
object Alt2 extends AltTrial {
  val a = OneOne[String]("a")
  def MAIN(args: Seq[String]): Unit = {
    (π { repeat { a ! readLine } }
      || π {
        serve {
          (a =?=> { line => if (line == null) stop else println(line) }
            | after(3 * Sec) ==> { println("*") })
        }
        a.close()
      })()

  }

}

/** <p>Forwards typed lines via forwarding channel `a` to a console-driving
  * serve loop. Closes a when . is typed, but continues to forward. The serve
  * loop prompts every three seconds, and will terminate when `a` is closed; but
  * the termination is marked by an `orelse` clause.
  */
object Alt3 extends AltTrial {
  val a = OneOne[String]("a")
  def MAIN(args: Seq[String]): Unit = {
    (π {
      repeat { val line = readLine; if (line == ".") a.close() else a ! line }
    }
      || π {
        serve {
          (a =?=> { line => println(line) }
            | after(3 * Sec) ==> { println("*") }
            | orelse ==> { println("ORELSE") })
        }
        a.close()
      })()

  }
}

/** This is for syntax-testing purposes */
object AltSyntax extends AltTrial {

  val a = OneOne[String]("a")
  val b = OneOne[String]("b")
  val c = OneOne[String]("c")
  val d = OneOne[String]("d")
  val e = OneOne[String]("e")
  val chans = for (i <- 0 until 1) yield OneOne[String](i.toString)

  val events = (((3 == 4) && a.outPort) =!=> { "foo" }
    | ((3 == 4) && b.inPort) =?=> { s => {} }
    | c.outPort =!=> { "foo" } ==> { println() }
    // | d.outPort=!=> {"foo"} ==> { s => println(s) }// this notation withdrawn
    | |(for (ch <- chans) yield (ch.outPort =!=> { "sent" }))
    | e.inPort =?=> { s => () }
    | d.outPort =!=> { "foo" })

  val events2 = ((3 == 4 && 5 == 6 && a) =!=> { "foo" }
    | (3 == 4 && 5 == 6 && b) =?=> { s => {} }
    | c =!=> { "foo" } ==> { println() }
    // | d =!=> {"foo"} ==> { s => println(s) }// this notation withdrawn
    | (|(for (ch <- chans) yield (ch =!=> { "sent" })))
    | e =?=> { s => () }
    | d =!=> { "foo" }
    | orelse ==> {})

  val events3 = ((|(for (ch <- chans) yield (ch =!=> { "sent" })))
    | e =?=> { s => () }
    | after(400) ==> {}
    | orelse ==> {})

  val events4 = ((|(for (ch <- chans) yield (ch =!=> { "sent" })))
    | e =?=> { s => () }
    | after(400) ==> {})

  /* Bad CSO grammar -- should be caught by the type checker

     val events5 =( d =!=> { "" }
                  | e =?=> { s => () }
                  | after(20) ==> {}
                  | orelse ==> {}
                  | orelse ==> {}

                 )
   */

  def MAIN(args: Seq[String]) = {
    println((1, events.toNormalizedString))
    println((2, events2.toNormalizedString))
    println((3, events3.toNormalizedString))
    println((4, events4.toNormalizedString))
  }

}
