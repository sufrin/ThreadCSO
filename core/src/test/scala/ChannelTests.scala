import io.SourceLocation.SourceLocation
import io.threadcso._
import io.threadcso.channel.OneOne
import io.threadcso.process.Stopped
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

/** Elementary trials of channel communication */
abstract class ChannelTest(implicit loc: SourceLocation) {

  def test(): Boolean = test(Array[String]())

  /** Define a test that return true if it passed, false otherwise */
  def test(args: Array[String]): Boolean
}

/** Check single communication over OneOne channel works. This also tests
  * closeOut and\or closeIn
  */
class Chan0 extends ChannelTest {
  private val x = Random.nextInt(Int.MaxValue)
  private val N = 1000
  private def upStream(chan: OneOne[Int]) = proc {
    chan ! x
    chan.closeOut()
  }
  private def downStream(chan: OneOne[Int]) = proc {
    assert(x == chan ? ())
    chan.closeIn()
  }

  def test(args: Array[String]): Boolean = {
    var okay = true
    for (i <- 0 until N if okay) {
      val chan = new OneOne[Int]("chan_" + i.toString)
      try {
        run(upStream(chan) || downStream(chan))
      } catch {
        case e: AssertionError => okay = false
        case e: Throwable      => { okay = false; println(e) }
      }
    }
    okay
  }
}

/**
  * Tests the invariant that messages sent through a OneOne channel arrives in
  * order.
  */
class Chan1 extends ChannelTest {
  private val N = 100
  private val reps = 100

  // send the contents of arr over the channel chan
  private def upStream(chan: OneOne[Int], arr: Array[Int]) = proc("up") {
    for (x <- arr) { chan ! x }
    chan.closeOut()
  }

  // receive the contents of channel chan and check the order with arr
  private def downStream(chan: OneOne[Int], arr: Array[Int]) = proc("down") {
    for (i <- 0 until arr.length) { assert(arr(i) == chan ? ()) }
    chan.closeIn()
  }

  def test(args: Array[String]): Boolean = {
    var okay = true
    for (_ <- 0 until reps) {
      val arr = Array.fill(N)(Random.nextInt(Int.MaxValue))
      val chan = new OneOne[Int]("chan")

      try {
        run(upStream(chan, arr) || downStream(chan, arr.clone))
      } catch {
        case e: AssertionError => okay = false
        case e: Throwable      => { okay = false; println(e) }
      }
    }
    okay
  }
}

/** Check channels throw Stopped properly */
class Chan2 extends ChannelTest {
  private val N = 100
  private val reps = 1000

  // send the contents of arr over the channel chan
  private def upStream(chan: OneOne[Int]) = proc("up") {
    chan.closeOut()
  }

  // receive the contents of channel chan and check the order with arr
  private def downStream(chan: OneOne[Int]) = proc("down") {
    var go = true
    while (go) {
      try {
        val x = chan ? ()
      } catch {
        case e: Throwable => { assert(e.isInstanceOf[Stopped]); go = false }
      }
    }
  }

  def test(args: Array[String]): Boolean = {
    var okay = true
    for (_ <- 0 until reps) {
      val chan = new OneOne[Int]("chan")

      try {
        run(upStream(chan) || downStream(chan))
      } catch {
        case e: AssertionError => okay = false
        case e: Throwable      => { okay = false; println(e) }
      }
    }
    okay
  }
}

///** Tests downstream transmission and closing and repeat/stop */
//object Chan2 extends ChannelTests {
//  def MAIN(args: Array[String]): Unit = {
//    val mid =
//      if (args contains ("-a")) OneOne[String]("mid")
//      else io.threadcso.channel.OneOne[String]("mid")
//    run(π {
//      repeat {
//        Console.print("> ")
//        val ln = readLine
//        if (ln == null || ln == "") stop
//        mid ! ln
//      }
//      mid.closeOut()
//    }
//      || π {
//        repeat {
//          Console.println(mid ? ())
//        }
//      })
//    System.exit(0)
//  }
//}

///** Tests downstream transmission and closing, and upstream closing */
////noinspection VarCouldBeVal
//object Chan3 extends ChannelTests {
//  def MAIN(args: Array[String]): Unit = {
//    val mid =
//      if (args contains ("-a")) OneOne[String]("mid")
//      else io.threadcso.channel.OneOne[String]("mid")
//    run(π {
//      var go = true
//      while (go) {
//        Console.print("> ")
//        var ln = readLine
//        if (ln == null || ln == "") go = false
//        mid ! ln // we expect this to fail after a "." has passed
//      }
//      mid.closeOut()
//    }
//      || π {
//        repeat {
//          val line = mid ? ()
//          Console.println(line)
//          if (line == ".") mid.closeIn()
//        }
//      })
//    System.exit(0)
//  }
//}

///** <p>Tests downstream transmission and closing, upstream closing; and extended
//  * rendezvous.
//  *
//  * Sender repeatedly prompts, reads a keyboard line, sends it to mid Listener
//  * reads from mid and (in an extended rendezvous) prints the line (without NL)
//  * pauses and prints a newline.
//  *
//  * Sender terminates at eof; receiver closes mid and terminates at "."
//  */
////noinspection VarCouldBeVal
//object Chan4 extends ChannelTests {
//  def MAIN(args: Array[String]): Unit = {
//    val mid =
//      if (args contains ("-a")) OneOne[String]("mid")
//      else io.threadcso.channel.OneOne[String]("mid")
//    run(π("Sender") {
//      var go = true
//      while (go) {
//        Console.print("> ")
//        var ln = readLine
//        if (ln == null || ln == "") go = false else mid ! ln
//      }
//      mid.close()
//    }
//      || π("Listener") {
//        repeat {
//          def showWait(s: String): String = {
//            Console.print(s); sleep(2 * Sec); Console.println("!"); s
//          }
//          val line = mid ?? showWait
//          if (line == ".") mid.closeIn()
//        }
//      })
//    System.exit(0)
//  }
//}

///** As Chan4 but with deadlines on the receiver read */
////noinspection VarCouldBeVal,VarCouldBeVal
//object Chan5 extends ChannelTests {
//  def MAIN(args: Array[String]): Unit = {
//    val mid =
//      if (args contains ("-a")) OneOne[String]("mid")
//      else io.threadcso.channel.OneOne[String]("mid")
//    var patience = 1000 * milliSec
//    run(π {
//      var go = true
//      while (go) {
//        Console.print("> ")
//        var ln = readLine
//        if (ln == null || ln == "") go = false else mid ! ln
//      }
//      mid.close()
//    }
//      || proc("Listener") {
//        var delayed = 0
//        repeat(delayed < 25) {
//          mid.readBefore(patience) match {
//            case None       => print("."); delayed += 1
//            case Some(line) => if (line == ".") mid.closeIn() else println(line)
//          }
//        }
//        mid.closeIn()
//        println("Stopped listening")
//      })
//    System.exit(0)
//  }
//}

///** <p> Various tests of OneOne or `N2N(writers, readers=1)` used as an
//  * intermediate channel between (possibly many) writers/senders and a listener
//  * process.
//  *
//  * The constant factor is that the listener process uses extended rendezvous,
//  * and thus keeps the {{{send-N}}} processes engaged for 500ms after they send.
//  * This is shorter than the time taken to start a concurrently-running
//  * {{{send-N}}}, and this means that a OneOne gets '''output-barged'''. Of
//  * course a {{{ManyOne}}} doesn't, for any contention to output to the channel
//  * is resolved in the synchronized wait for !.
//  * {{{
//  * //
//  * // -s sequential sending; otherwise sends are in parallel
//  * // -m use N2N(writers=0, readers=1) channel; otherwise uses a OneOne channel
//  * // -n use N2N(writers=#args, 1) channel; otherwise use a OneOne channel
//  * // -o use close of mid after sending; otherwise uses closeout
//  * // Here we describe what happens when #args>1
//  * // -m -o   args -- terminates
//  * // -m      args -- hangs in Listener (and MAIN) after whole sender terminates
//  * // -n      args -- terminates
//  * // -s [-o] args -- terminates if -o is set, else hangs in Listener
//  * //         args -- throws output-barging exceptions and hangs in Listener
//  * }}}
//  */
//object Chan6 extends ChannelTests {

//  def MAIN(args: Array[String]): Unit = {
//    val mid: Chan[String] =
//      if (args contains "-m")
//        N2N(writers = 0, readers = 1, "mid")
//      else if (args contains "-n")
//        N2N(args.length, readers = 1, "mid")
//      else
//        OneOne("mid")
//    val sendPar = ||(for (arg <- args) yield π("send-" + arg) { mid ! arg })
//    val sendParClose = ||(for (arg <- args) yield π("send-" + arg) {
//      mid ! arg; mid.closeOut()
//    })
//    val sendSeq = π { for (arg <- args) π("send-" + arg) { mid ! arg }() }
//    val send =
//      if (args contains "-s") sendSeq
//      else if (args contains "-n") sendParClose
//      else sendPar
//    run(π {
//      send()
//      Console.println("---")
//      if (args contains "-o") mid.close()
//    }

//      || π("Listener") {
//        repeat {
//          def showWait(s: String): String = {
//            Console.print(s); sleepms(500); Console.println(""); s
//          }
//          val line = mid ?? showWait
//          if (line == ".") mid.closeIn()
//        }
//      })
//    System.exit(0)
//  }
//}

///** <p>The near-"dual" of Chan6 -- receiving is multiplexed.
//  * {{{
//  * //
//  * // -s sequential (not concurrent) receiving
//  * // -m OneMany (not OneOne) channel
//  * // +  Force a single extra read in the Listener (for attempt)
//  * // Here we describe what we expect to happen:
//  * // -m args -- terminates
//  * // -s args -- terminates
//  * //    args -- if there are several args then it is likely that
//  * //            the Listener will terminate with a ParException composed
//  * //            of the IllegalState exceptions thrown by the overtaking
//  * //            reading processes.
//  * }}}
//  */
//object Chan7 extends ChannelTests {

//  def MAIN(args: Array[String]): Unit = {
//    def showWait(s: String): String = {
//      Console.print(s); sleep(500); Console.println(""); s
//    }
//    val mid: Chan[String] =
//      if (args contains "-m")
//        N2N[String](readers = 1, writers = 0, name = "mid")
//      else
//        OneOne[String]("mid")
//    val recPar =
//      (||(for (arg <- args) yield π("rec-" + arg) { showWait(mid ? ()) }))
//    val recSeq = π {
//      for (arg <- args) run(π("rec-" + arg) { showWait(mid ? ()) })
//    }
//    run(π {
//      for (arg <- args) mid ! arg
//      Console.println("---")
//      mid.closeOut()
//    }

//      || π("Listener") {
//        attempt {
//          if (args contains "-s") recSeq() else recPar()
//          Console.println("***")
//          if (args contains "+") showWait(mid ? ())
//        } {
//          Console.println("******")
//        }
//        mid.close()
//      })
//    System.exit(0)
//  }
//}

///** Makes a cyclic live/deadlock so as to test the debugger */
//object Deadlock extends ChannelTests {
//  def fwd(in: ??[String], out: !![String]): PROC =
//    proc(s"forward ${in.name} to ${out.name}") {
//      repeat { val v = in ? (); Console.println(in.name); out ! v }
//    }
//  def MAIN(args: Array[String]): Unit = {
//    val chans = for (arg <- args) yield OneOne[String](arg)
//    val procs = ||(
//      for (i <- chans.indices)
//        yield fwd(chans(i), chans((i + 1) % chans.length))
//    )
//    if (args(0) == "live") proc { chans(0) ! "_x" }.fork
//    run(procs)
//  }
//}

// private val N = 100

// private val alpha =
//   "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

// protected def randStr(M: Int) = {
//   val n = Random.nextInt(M) + 2
//   (1 to n).map(_ => alpha(Random.nextInt(alpha.length))).mkString
// }

class ChannelTests extends AnyFlatSpec {

  behavior of "OneOne channels"

  it should "send one input to the output" in {
    val ct = new Chan0
    assert(ct.test())
  }

  it should "send multiple inputs in order" in {
    val ct = new Chan1
    assert(ct.test())
  }

  it should "throw Stopped upon inPort closing" in {
    val ct = new Chan2
    assert(ct.test())
  }
}
