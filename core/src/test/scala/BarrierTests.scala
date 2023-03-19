import org.scalatest.flatspec.AnyFlatSpec
import io.threadcso.debug.Log
import io.threadcso.lock.GenericBarrier
import io.threadcso._
import scala.util.Random

// Events to place in Log
sealed trait LogEvent
// Thread id calls sync or returns from sync
case class Arrive(id: Int) extends LogEvent
case class Leave(id: Int) extends LogEvent

sealed trait BarrierConstructor {
  def apply(p: Int): GenericBarrier[Unit]
}

object LinearBarrierCons extends BarrierConstructor {
  def apply(p: Int) = Barrier(p)
}

object LogBarrierCons extends BarrierConstructor {
  def apply(p: Int) = LogBarrier(p)
}

class BarrierSync(cons: BarrierConstructor) {
  val iters = 100 // # iterations per test
  val reps = 500

  /** Run a single test. */
  def doTest = {
    val p = 1 + Random.nextInt(20) // # threads
    val barrier = cons(p)
    val log = new Log[LogEvent](p)
    def worker(me: Int) = proc(f"barrier_$me") {
      for (_ <- 0 until iters) {
        log.add(me, Arrive(me))
        barrier.sync(me, ())
        log.add(me, Leave(me))
      }
    }
    run(||(for (i <- 0 until p) yield worker(i)))
    // barrier.shutdown
    checkLog(log.get, p)
  }

  /** Check that es represents a valid history for p threads. */
  def checkLog(events: Array[LogEvent], p: Int) = {
    // We traverse the log, keeping track of which threads are currently
    // within a sync, respectively waiting to synchronise or leaving; we use a
    // bitmap for each of these sets.
    var waiting, leaving = new Array[Boolean](p)
    var numWaiting, numLeaving = 0 // # currently waiting, leaving
    for (event <- events) {
      event match {
        case Arrive(id) =>
          assert(!waiting(id))
          waiting(id) = true
          numWaiting += 1
          if (numWaiting == p) { // all now can leave
            assert(numLeaving == 0)

            leaving = waiting
            numLeaving = p
            waiting = new Array[Boolean](p)
            numWaiting = 0
          }
        case Leave(id) =>
          assert(leaving(id))

          leaving(id) = false
          numLeaving -= 1
      }
    }
  }

  def test() = {
    for (r <- 0 until reps) { doTest }
  }
}

class BarrierTests extends AnyFlatSpec {

  behavior of "LinearBarrier"

  it should "synchronization should work" in {
    val sync = new BarrierSync(LinearBarrierCons)
    sync.test()
    // assert(ct.test())
  }

  behavior of "LogBarrier"

  it should "synchronization should work" in {
    val sync = new BarrierSync(LogBarrierCons)
    sync.test()
    // assert(ct.test())
  }

}
