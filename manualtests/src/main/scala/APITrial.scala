import io.SourceLocation.SourceLocation
import io.threadcso._

/** <p> Elementary trials of concurrent composition, thread-pooling, thread
  * stacksize specification.
  *
  * Also Primes:
  * {{{
  * -for=N -- count of primes before stopping
  * -max=N -- largest number tested for primality before stopping
  * -c     -- show closing of intermediate channel
  * -a=N   -- set stack radius of source and nomult and sink threads
  * -b=N   -- set stack radius of (recursive) sieve threads
  * -p=N   -- pause after N primes (to test the debugger)
  * -q     -- quietly (don't show the primes)
  * -Kpoolkind -- set the default source of threads (VIRTUAL)
  * }}}
  */
abstract class APITrial(implicit loc: SourceLocation) {
  def readLine: String = scala.io.StdIn.readLine()
  val shutDown: Thread = new Thread() {
    override def run(): Unit = { Console.println("Shut down") }
  }

  def MAIN(args: Array[String]): Unit

  // This forces the main cso program into a non-main thread
  // It's for no particular reason
  def main(args: Array[String]): Unit = {
    val realArgs = args.filter { case arg =>
      if (arg.startsWith("-K")) {
        val poolKind = arg.substring(2)
        println(s"Pool kind: $poolKind")
        scala.util.Properties.setProp("io.threadcso.pool.KIND", poolKind)
        false
      } else
        true
    }
    run(proc("Runtime System") {} || proc("MAIN") { MAIN(realArgs.toArray) })
  }

  java.lang.Runtime.getRuntime.addShutdownHook(shutDown)

  println(s"Runnable $loc")
  println(debugger)

}

/** Tests PAR normal and exceptional termination */
object PAR extends APITrial {
  def MAIN(args: Array[String]): Unit = {
    val p = ||(for (arg <- args) yield proc(arg) {
      Console.println(arg)
      sleep(1000)
      if (arg == ".") stop
      else if (arg.startsWith(".")) throw new Throwable(arg)
    })
    Console.println(p)
    p()
    Console.println("------------")
    p()
    Console.println("------------")
    par(List(p, p))
    Console.println("------------")
    run(p || p)
    // System.exit(0)
  }
}

/** Tests PAR syntax and normal and exceptional termination */
object PARSyntax extends APITrial {
  def MAIN(args: Array[String]): Unit = {
    var p = SKIP
    for (arg <- args)
      p = p ||
        proc(arg) {
          Console.println(arg)
          sleep(1000)
          if (arg == ".") stop
          else if (arg.startsWith(".")) throw new Throwable(arg)
        }

    p()
    Console.println("------------")
    p()
    Console.println("------------")
    par(List(p, p))
    Console.println("------------\n" + (p || p).toString)
    (p || p)()
    System.exit(0)
  }
}

/** Tests PAR syntax, process stacksize, and normal and exceptional termination
  */
object PARStack extends APITrial {
  def MAIN(args: Array[String]): Unit = {
    var p = SKIP
    var size = 1L << 6
    for (arg <- args) {
      val s = size
      p = p ||
        proc(arg) {
          Console.println((s, arg))
          if (arg == ".") stop
          else if (arg.startsWith(".")) throw new Throwable(arg)
        }.withStackSize(s)
      size = size << 2
    }
    p()
    Console.println("------------")
    p()
    Console.println("------------")
    par(List(p, p))
    Console.println("------------\n" + (p || p).toString)
    (p || p)()
    System.exit(0)
  }
}

/** Primes generator: tests propagation of forward and backward close, and of
  * kernel thread-creation limits
  *
  * -for=N -- count of primes before stopping
  * -max=N -- largest number tested for primality before stopping
  * -c -- show closing of intermediate channel
  * -a=N -- set stack size of source and nomult and sink threads
  * -b=N -- set stack size of (recursive) sieve threads
  * -p=N -- pause after N primes (to test the debugger)
  * -q -- quietly
  *
  * Benchmarks from 2017
  * {{{
  *  Wed  4 Oct 2017 13:07:06 BS
  *  OS X (Sierra) 2.9ghz Intel Core I7 (8G DDR3 1600mhz) 17657 (2028th prime) 36secs
  * (cause: OS hard limit on kthreads)
  * }}}
  *
  * Some historical benchmarks:
  * {{{
  * OS              Hardware                     Max Prime Reached
  * --------------------------------------------------------------------
  * --------------------------------------------------------------------
  * Date: Tue Feb 28 18:12:59 GMT 2023 USING LOOM (lightweight threads) JDK20
  * OS X Monterey
  * (12.6.3)        Intel Mac Mini (2018)
  *                 32gb 3.2Ghz 6-core i7       49999th is 611951
  *                                             50k primes in 126s -- 240s inc pipeline closedown
  *                                             9999th is 104723
  *                                             10k primes in 4.8s -- 11s inc pipeline closedown
  * --------------------------------------------------------------------
  * Date: Fri 15 Nov 2013 19:02:24 GMT
  * MAC OS X Lion 4GB Intel Macbook Air    STOPPED AT(~2300th prime 11 minutes)
  * Ubuntu Server 1TB Intel 80CPU          STILL GOING (9591 primes in 1m10.8s)
  * [Same program in GO on the above machine]
  * STILL GOING (9591 primes in 16.8s)
  * Fedora machines now seem to impose a limit that results in termination
  * after generating only 1002 primes.
  * --------------------------------------------------------------------
  * Date: 2008-09-17 17:22:11 +0100 (Wed, 17 Sep 2008) java 1.5
  * MAC OS X (Tiger)1GB PPC Mac Mini             22811   (2549th prime)
  * MAC OS X    "   2GB 1x2.00ghz Core Duo       22811   (2549th prime)
  * MAC OS X    "   4GB 2x2.66ghz Dual Core Xeon 22811   (2549th prime)
  * Suse 9.2     0.75GB 1.8ghz IBM T30 (x86)     29027   (3158th prime)
  * Fedora          2gb 2x AMD64X2 x86.64        179041  (16252nd prime)
  * ---------------------------------------------------------------------
  * MAC OS X (Snow Leopard) java 1.6
  * 4GB 2x2.66ghz Dual Core Xeon 22739   (2541st prime)
  * [this was an immature port to scala 2.8.0]
  * }}}
  */
object Primes extends APITrial {
  var showClose = false
  var aSize = 0L
  var bSize = 0L

  def MAIN(args: Array[String]): Unit = {
    var max = 0
    var count = 0
    var pause = 0
    var quiet = false
    for (arg <- args)
      if (arg.startsWith("-max="))
        max = arg.substring(5).toInt
      else if (arg.startsWith("-for="))
        count = arg.substring(5).toInt
      else if (arg.startsWith("-c"))
        showClose = true
      else if (arg.startsWith("-a="))
        aSize = arg.substring(3).toLong
      else if (arg.startsWith("-b="))
        bSize = arg.substring(3).toLong
      else if (arg.startsWith("-p="))
        pause = arg.substring(3).toInt
      else if (arg == "-d") println(debugger)
      else if (arg == "-q") quiet = true
      else
        Console.println(
          s"($arg) -max=... -for=... | -a=<N> | -b=<N> | -c | -d | -q | -p=<N>"
        )

    val mid = io.threadcso.channel.OneOne[Int]("mid")
    val res = io.threadcso.channel.OneOne[Int]("res")
    try {
      (
        sieve("seive")(mid, res)
          || proc("output") {
            val start = java.lang.System.currentTimeMillis()
            var i = 1
            var prime = 0
            repeat {
              prime = res ? ()
              if (!quiet) System.out.println("%04d %5d".format(i, prime))
              i = i + 1
              if (i == count) stop
              if (pause != 0 && i > pause) {
                Console.print("<Enter> to continue: "); readLine; ()
              }
            }
            val elapsed = java.lang.System.currentTimeMillis() - start
            Console.println(s"FINISHED: $i: $prime (in $elapsed milliseconds)")
            res.closeOut()
          }.withStackSize(aSize)
          || proc("source") {
            var i = 2;
            repeat { mid ! i; i = i + 1; if (i == max) mid.closeIn() }
          }.withStackSize(aSize)
      )()
    } finally {
      System.exit(0)
    }
  }

  def noMult(n: Int, in: channel.??[Int], out: channel.!![Int]): PROC =
    proc("nomult" + n.toString) {
      repeat {
        val m = in ? ()
        if (m % n != 0) out ! m
      }
      if (showClose) Console.println("nomult " + n.toString + " closing")
      in.closeIn()
      out.closeOut()
      if (showClose) Console.println("nomult " + n.toString + " closed")
    }.withStackSize(aSize)

  def sieve(name: String)(in: channel.??[Int], out: channel.!![Int]): PROC =
    proc(name) {
      val n = in ? ()
      val mid = io.threadcso.channel.OneOne[Int]("Mid-%d".format(n))
      attempt {
        out ! n
        (noMult(n, in, mid) || sieve(s"seive-$n")(mid, out))()
        if (showClose)
          Console.println("(nomult(%d}||sieve ...)() finished".format(n))
        in.closeIn()
        mid.close()
        out.closeOut()
      } { // Here if the output port is closed
        if (showClose) Console.println("sieve " + n + " closing")
        mid.close()
        in.closeIn()
        if (showClose) Console.println("sieve " + n + " closed")
      }
    }.withStackSize(bSize)
}

/** Tests downstream transmission and closing */
// object Chan1 extends APITrial {
//   def MAIN(args: Array[String]): Unit = {
//     val mid =
//       if (args contains ("-a")) OneOne[String]("mid")
//       else io.threadcso.channel.OneOne[String]("mid")
//     run(π { for (a <- args) mid ! a; mid.closeOut() }
//       || π { repeat { Console.println(mid ? ()) } })
//     System.exit(0)
//   }
// }

/** Tests downstream transmission and closing and repeat/stop */
// object Chan2 extends APITrial {
//   def MAIN(args: Array[String]): Unit = {
//     val mid =
//       if (args contains ("-a")) OneOne[String]("mid")
//       else io.threadcso.channel.OneOne[String]("mid")
//     run(π {
//       repeat {
//         Console.print("> ")
//         val ln = readLine
//         if (ln == null || ln == "") stop
//         mid ! ln
//       }
//       mid.closeOut()
//     }
//       || π {
//         repeat {
//           Console.println(mid ? ())
//         }
//       })
//     System.exit(0)
//   }
// }

/** Tests downstream transmission and closing, and upstream closing */
//noinspection VarCouldBeVal
object Chan3 extends APITrial {
  def MAIN(args: Array[String]): Unit = {
    val mid =
      if (args contains ("-a")) OneOne[String]("mid")
      else io.threadcso.channel.OneOne[String]("mid")
    run(π {
      var go = true
      while (go) {
        Console.print("> ")
        var ln = readLine
        if (ln == null || ln == "") go = false
        mid ! ln // we expect this to fail after a "." has passed
      }
      mid.closeOut()
    }
      || π {
        repeat {
          val line = mid ? ()
          Console.println(line)
          if (line == ".") mid.closeIn()
        }
      })
    System.exit(0)
  }
}

/** <p>Tests downstream transmission and closing, upstream closing; and extended
  * rendezvous.
  *
  * Sender repeatedly prompts, reads a keyboard line, sends it to mid Listener
  * reads from mid and (in an extended rendezvous) prints the line (without NL)
  * pauses and prints a newline.
  *
  * Sender terminates at eof; receiver closes mid and terminates at "."
  */
//noinspection VarCouldBeVal
object Chan4 extends APITrial {
  def MAIN(args: Array[String]): Unit = {
    val mid =
      if (args contains ("-a")) OneOne[String]("mid")
      else io.threadcso.channel.OneOne[String]("mid")
    run(π("Sender") {
      var go = true
      while (go) {
        Console.print("> ")
        var ln = readLine
        if (ln == null || ln == "") go = false else mid ! ln
      }
      mid.close()
    }
      || π("Listener") {
        repeat {
          def showWait(s: String): String = {
            Console.print(s); sleep(2 * Sec); Console.println("!"); s
          }
          val line = mid ?? showWait
          if (line == ".") mid.closeIn()
        }
      })
    System.exit(0)
  }
}

/** As Chan4 but with deadlines on the receiver read */
//noinspection VarCouldBeVal,VarCouldBeVal
object Chan5 extends APITrial {
  def MAIN(args: Array[String]): Unit = {
    val mid =
      if (args contains ("-a")) OneOne[String]("mid")
      else io.threadcso.channel.OneOne[String]("mid")
    var patience = 1000 * milliSec
    run(π {
      var go = true
      while (go) {
        Console.print("> ")
        var ln = readLine
        if (ln == null || ln == "") go = false else mid ! ln
      }
      mid.close()
    }
      || proc("Listener") {
        var delayed = 0
        repeat(delayed < 25) {
          mid.readBefore(patience) match {
            case None       => print("."); delayed += 1
            case Some(line) => if (line == ".") mid.closeIn() else println(line)
          }
        }
        mid.closeIn()
        println("Stopped listening")
      })
    System.exit(0)
  }
}

/** <p> Various tests of OneOne or `N2N(writers, readers=1)` used as an
  * intermediate channel between (possibly many) writers/senders and a listener
  * process.
  *
  * The constant factor is that the listener process uses extended rendezvous,
  * and thus keeps the {{{send-N}}} processes engaged for 500ms after they send.
  * This is shorter than the time taken to start a concurrently-running
  * {{{send-N}}}, and this means that a OneOne gets '''output-barged'''. Of
  * course a {{{ManyOne}}} doesn't, for any contention to output to the channel
  * is resolved in the synchronized wait for !.
  * {{{
  * //
  * // -s sequential sending; otherwise sends are in parallel
  * // -m use N2N(writers=0, readers=1) channel; otherwise uses a OneOne channel
  * // -n use N2N(writers=#args, 1) channel; otherwise use a OneOne channel
  * // -o use close of mid after sending; otherwise uses closeout
  * // Here we describe what happens when #args>1
  * // -m -o   args -- terminates
  * // -m      args -- hangs in Listener (and MAIN) after whole sender terminates
  * // -n      args -- terminates
  * // -s [-o] args -- terminates if -o is set, else hangs in Listener
  * //         args -- throws output-barging exceptions and hangs in Listener
  * }}}
  */
object Chan6 extends APITrial {

  def MAIN(args: Array[String]): Unit = {
    val mid: Chan[String] =
      if (args contains "-m")
        N2N(writers = 0, readers = 1, "mid")
      else if (args contains "-n")
        N2N(args.length, readers = 1, "mid")
      else
        OneOne("mid")
    val sendPar = ||(for (arg <- args) yield π("send-" + arg) { mid ! arg })
    val sendParClose = ||(for (arg <- args) yield π("send-" + arg) {
      mid ! arg; mid.closeOut()
    })
    val sendSeq = π { for (arg <- args) π("send-" + arg) { mid ! arg }() }
    val send =
      if (args contains "-s") sendSeq
      else if (args contains "-n") sendParClose
      else sendPar
    run(π {
      send()
      Console.println("---")
      if (args contains "-o") mid.close()
    }

      || π("Listener") {
        repeat {
          def showWait(s: String): String = {
            Console.print(s); sleepms(500); Console.println(""); s
          }
          val line = mid ?? showWait
          if (line == ".") mid.closeIn()
        }
      })
    System.exit(0)
  }
}

/** <p>The near-"dual" of Chan6 -- receiving is multiplexed.
  * {{{
  * //
  * // -s sequential (not concurrent) receiving
  * // -m OneMany (not OneOne) channel
  * // +  Force a single extra read in the Listener (for attempt)
  * // Here we describe what we expect to happen:
  * // -m args -- terminates
  * // -s args -- terminates
  * //    args -- if there are several args then it is likely that
  * //            the Listener will terminate with a ParException composed
  * //            of the IllegalState exceptions thrown by the overtaking
  * //            reading processes.
  * }}}
  */
object Chan7 extends APITrial {

  def MAIN(args: Array[String]): Unit = {
    def showWait(s: String): String = {
      Console.print(s); sleep(500); Console.println(""); s
    }
    val mid: Chan[String] =
      if (args contains "-m")
        N2N[String](readers = 1, writers = 0, name = "mid")
      else
        OneOne[String]("mid")
    val recPar =
      (||(for (arg <- args) yield π("rec-" + arg) { showWait(mid ? ()) }))
    val recSeq = π {
      for (arg <- args) run(π("rec-" + arg) { showWait(mid ? ()) })
    }
    run(π {
      for (arg <- args) mid ! arg
      Console.println("---")
      mid.closeOut()
    }

      || π("Listener") {
        attempt {
          if (args contains "-s") recSeq() else recPar()
          Console.println("***")
          if (args contains "+") showWait(mid ? ())
        } {
          Console.println("******")
        }
        mid.close()
      })
    System.exit(0)
  }
}

/** Makes a cyclic live/deadlock so as to test the debugger */
object Deadlock extends APITrial {
  def fwd(in: ??[String], out: !![String]): PROC =
    proc(s"forward ${in.name} to ${out.name}") {
      repeat { val v = in ? (); Console.println(in.name); out ! v }
    }
  def MAIN(args: Array[String]): Unit = {
    val chans = for (arg <- args) yield OneOne[String](arg)
    val procs = ||(
      for (i <- chans.indices)
        yield fwd(chans(i), chans((i + 1) % chans.length))
    )
    if (args(0) == "live") proc { chans(0) ! "_x" }.fork
    run(procs)
  }
}

/** <p> Test transitivity of extended rendezvous. Writer sends a * into a
  * pipeline of forwarding processes (of length the number of arguments). Reader
  * reads from the end of the pipeline, then awaits input from the console.
  * Writer shows it has synchronized by printing #. then the pipeline of
  * forwarders closes down.
  */
object Rendezvous extends APITrial {
  def fwd(me: String)(in: ??[String], out: !![String]): PROC =
    proc(s"fwd($me)") {
      repeat { in ?? (s => out ! s) }
      println(s"$me finishing")
      in.closeIn()
      out.closeOut()
    }
  def MAIN(args: Array[String]): Unit = {
    val left = OneOne[String]("LEFT")
    val right = OneOne[String]("RIGHT")
    val chans =
      List(left) ++ args.map(arg => OneOne[String](arg)) ++ List(right)
    val procs = ||(
      for (i <- 0 until chans.length - 1)
        yield fwd(s"${chans(i).name} -> ${chans(i + 1).name}")(
          chans(i),
          chans(i + 1)
        )
    )
    run(
      procs
        || π("Writer") { left ! "*"; Console.println("#"); left.closeOut() }
        || π("Reader") {
          right ?? { s => Console.println(s"$s"); readLine };
          println("Reader finished")
        }
    )
    System.exit(0)
  }
}

/** <p> Deadlocks with channel open. Debugger lets you see their names. */
object Names extends APITrial {
  def MAIN(args: Array[String]): Unit = {
    val a = io.threadcso.channel.N2N[String](3, 3, "a")
    val b = io.threadcso.channel.N2N.ManyOne[String](name = "b")
    val c = io.threadcso.channel.N2N.OneMany[String](name = "c")
    val d = io.threadcso.channel.N2N.ManyMany[String](name = "d")
    val e = OneOneBuf[String](20, "e")

    run(||(for (ch <- List(a, b, c, d, e)) yield π { ch ? (); () }))
  }
}

/** <p> Basic buffer implementation test for use with debugger.
  *
  * -s args -- sequential output of args into buffer (>10 will deadlock)
  * followed by sequential input from buffer args -- concurrent output and input
  * of args; terminates at end of args.
  * -step args -- as above; but prompt for <enter> at each output. Receiver does
  * a closeInp on receiving a . When not -stepping sender is likely to get more
  * data into the buffer than when stepping.
  */
object SyncBuf extends APITrial {
  def MAIN(args: Array[String]): Unit = {
    val b = OneOneBuf[String](10)
    val step = args contains "-step"
    val sender = π("sender") {
      repeatFor(args)(b ! _)
      b.closeOut()
    }
    val receiver = π("receiver") {
      repeat {
        val s = b ? ()
        print(s"$s: ")
        if (s == ".") b.closeIn()
        if (step) readLine else println()
        ()
      }
    }
    if (args contains "-s") { sender(); receiver() }
    else
      run(sender || receiver)
    System.exit(0)
  }
}
