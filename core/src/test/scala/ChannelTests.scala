import io.SourceLocation.SourceLocation
import io.threadcso._
import io.threadcso.channel.OneOne
import io.threadcso.process.Stopped
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

/** Trials of channel communication (slowly being extended)
  */

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
  private def sender(chan: OneOne[Int]) = proc {
    chan ! x
    chan.closeOut()
  }
  private def receiver(chan: OneOne[Int]) = proc {
    assert(x == chan ? ())
    chan.closeIn()
  }

  def test(args: Array[String]): Boolean = {
    var okay = true
    for (i <- 0 until N if okay) {
      val chan = new OneOne[Int]("chan_" + i.toString)
      try {
        run(sender(chan) || receiver(chan))
      } catch {
        case e: AssertionError => okay = false
        case e: Throwable      => { okay = false; println(e) }
      }
    }
    okay
  }
}

/** Tests the invariant that messages sent through a OneOne channel arrives in
  * order.
  */
class Chan1 extends ChannelTest {
  private val N = 100
  private val reps = 100

  // send the contents of arr over the channel chan
  private def sender(chan: OneOne[Int], arr: Array[Int]) = proc("up") {
    for (x <- arr) { chan ! x }
    chan.closeOut()
  }

  // receive the contents of channel chan and check the order with arr
  private def receiver(chan: OneOne[Int], arr: Array[Int]) = proc("down") {
    for (i <- 0 until arr.length) { assert(arr(i) == chan ? ()) }
    chan.closeIn()
  }

  def test(args: Array[String]): Boolean = {
    var okay = true
    for (_ <- 0 until reps) {
      val arr = Array.fill(N)(Random.nextInt(Int.MaxValue))
      val chan = new OneOne[Int]("chan")

      try {
        run(sender(chan, arr) || receiver(chan, arr.clone))
      } catch {
        case e: AssertionError => okay = false
        case e: Throwable      => { okay = false; println(e) }
      }
    }
    okay
  }
}

/** Check OneOne channels throw Stopped properly
  */
class Chan2 extends ChannelTest {
  private val N = 100
  private val reps = 1000

  // send the contents of arr over the channel chan
  private def sender(chan: OneOne[Int]) = proc("sender") {
    chan.closeOut()
  }

  // receive the contents of channel chan and check the order with arr
  private def receiver(chan: OneOne[Int]) = proc("receiver") {
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
        run(sender(chan) || receiver(chan))
      } catch {
        case e: AssertionError => okay = false
        case e: Throwable      => { okay = false; println(e) }
      }
    }
    okay
  }
}

/** Check N2NBuf channels throw Stopped properly on write-closing
  */
class ChanN2NWrite extends ChannelTest {
  private val N = 100
  private val reps = 5
  private val writers = 10

  // send the contents of arr over the channel chan
  private def writer(chan: !![Int]) = proc("writer") {
    chan ! 3
    chan.closeOut()
  }

  // receive the contents of channel chan and check the order with arr
  private def receiver(chan: ??[Int]) = proc("receiver") {
    var go = true
    while (go) {
      try {
        val x = chan ? ()
      } catch {
        case e: Throwable => { assert(e.isInstanceOf[Stopped]); go = false }
      }
    }
    chan.closeIn()
  }

  def test(args: Array[String]): Boolean = {
    println(debugger)
    var okay = true
    for (trial <- 0 until reps) {

      val chan = N2NBuf[Int](10, writers = writers, readers = 1)
      if (trial == 0) println(s"Before Trial $trial\nChan state: $chan")

      try {
        val senders = ||(for { i <- 0 until writers } yield writer(chan))
        run(senders || receiver(chan))
      } catch {
        case e: AssertionError => okay = false
        case e: Throwable      => { okay = false; println(e) }
      }
      if (trial == 0) println(s"After Trial $trial\nChan state: $chan")

    }
    okay
  }
}

class ChanCopy extends ChannelTest {
  // uses the standard OneOne and N2NBuf factories
  private val N = 5000
  private val reps = 100

  // send the contents of arr over the channel chan
  private def sender(writers: Int, chan: !![Int], arr: Array[Int]) =
    proc("up") {
      for (x <- arr) { chan ! x }
      for (_ <- 0 until writers) chan.closeOut()
    }

  // receive the contents of channel chan and check the order with arr
  private def receiver(chan: ??[Int], dest: Array[Int]) = proc("down") {
    var i = 0
    repeat {
      dest(i) = chan ? ()
      i += 1
    }
    assert(i == N)
  }

  def test(args: Array[String]): Boolean = {
    val closers = if (args contains "3") 3 else 1

    for (trial <- 0 until reps) {

      val source = Array.fill(N)(Random.nextInt(Int.MaxValue))
      val dest = Array.fill(N)(0)

      val chan =
        if (args contains "n2n")
          io.threadcso.N2NBuf[Int](10, readers = 1, writers = closers)
        else
          io.threadcso.OneOne[Int]("chan")

      run(sender(writers = closers, chan, source) || receiver(chan, dest))

      for { i <- 0 until N } assert(dest(i) == source(i))

      if (trial == 0) println(s"After Trial $trial\nChan state: $chan")
    }
    true
  }
}

/*class TransmissionDownstream {
      def test(args: Array[String]): Unit = {
        val mid =
          if (args contains ("-a")) OneOne[String]("mid")
          else io.threadcso.channel.OneOne[String]("mid")
        run(π {
          repeat {
            Console.print("> ")
            val ln = readLine
            if (ln == null || ln == "") stop
            mid ! ln
          }
          mid.closeOut()
        }
          || π {
            repeat {
              Console.println(mid ? ())
            }
          })
        System.exit(0)
      }
}
 */

class ChannelTests extends AnyFlatSpec {

  behavior of "N2N channels"

  it should "throw Stopped upon outPort of an N2NBuf(...writers=4...) closing" in {
    val ct = new ChanN2NWrite
    assert(ct.test())
  }

  behavior of "OneOne channels"

  it should "send one input to the output" in {
    val ct = new Chan0
    assert(ct.test())
  }

  it should "send multiple inputs in order" in {
    val ct = new Chan1
    assert(ct.test())
  }

  it should "throw Stopped upon outPort of a OneOne closing" in {
    val ct = new Chan2
    assert(ct.test())
  }

  it should "have copied elements in a repeat using a channel made by the standard OneOne" in {
    val ct = new ChanCopy
    assert(ct.test())
  }

  it should "have copied elements in a repeat using a channel made by a standard N2NBUF(10, 1, 1)" in {
    val ct = new ChanCopy
    assert(ct.test(Array("n2n")))
  }

  it should "have copied elements in a repeat using a channel made by a standard N2NBUF(10, 3, 3)" in {
    val ct = new ChanCopy
    assert(ct.test(Array("n2n", "3")))
  }

}

class PrimitiveTests extends AnyFlatSpec {
  import io.threadcso._
  import io.threadcso.lock.primitive.{UnitChan, DataChan}

  def testUnit(N: Int = 500): Unit = {
    println(debugger)
    val sync = new UnitChan()
    var n = 0
    val inc = proc("inc") {
      for { i <- 0 until N } {
        sync.read(); n = n + 1; sync.write(())
      }
    }
    val dec = proc("dec") {
      for { i <- 0 until N } {
        sync.read(); n = n - 1; sync.write(())
      }
    }
    sync ! () // allow the first
    val s = nanoTime
    (dec || inc)()
    val e = (nanoTime - s).toDouble
    val tot = e / 1.0e9
    val per = e / N.toDouble
    println(s"N=$N in $tot secs ($per ns / communication)")
    assert(n == 0)
  }

  def testData(N: Int = 500): Unit = {
    println(debugger)
    val data = DataChan[Int]()

    var n = 0 // owned by reader

    val reader = proc("reader") {
      var going = true
      while (going) {
        data ? { r => n = r }
        going = n >= 0
      }
    }

    val writer = proc("writer") {
      for { i <- 0 to N } {
        if (i % 100 == 0) {
          // println(s"w$i")
          data ! i
        }
        data.write(i)
      }
      assert(n == N, s"n is $n; should be $N")
      data ! (-1)
    }

    val s = nanoTime
    (writer || reader)()
    val e = (nanoTime - s).toDouble
    val tot = e / 1.0e9
    val per = e / N.toDouble
    println(s"N=$N in $tot secs ($per ns / communication)")
    assert(n == -1)
  }

  it should "have run N*{ sync?(); n+=1; sync!() } || N*{ sync?(); n-=1; sync!() } for sync:UnitChan" in {
    testUnit(10000)
    testUnit(20000)
  }

  it should "have transmitted N integers using a DataChan[Int]" in {
    testData(10000)
    testData(20000)
  }
}
