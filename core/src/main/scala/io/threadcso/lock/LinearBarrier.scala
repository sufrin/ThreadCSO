package io.threadcso.lock

import io.threadcso.semaphore.BooleanSemaphore

/** A `LinearBarrier(n)` supports the ''repeated'' synchronization of `n`
  * processes.
  *
  * If `b` is such a barrier then `b.sync` calls are stalled until `n` have been
  * made. When `n==1` then `b.sync` returns immediately: this is so multi-worker
  * structures can be tested with only a single-worker, and may be helpful when
  * testing cellular automata.
  *
  * This implementation is simple-minded.
  */
abstract class LinearBarrier[T](
    private val n: Int,
    private val e: T,
    private val op: (T, T) => T,
    val name: String = ""
) extends Barrier[T] {
  assert(n >= 1)

  private[this] val shared = n > 1
  private[this] var waiting = 0 // number of processes currently waiting

  private[this] val wait =
    BooleanSemaphore(available = false, name = s"$name.wait")

  private[this] val enter =
    BooleanSemaphore(available = true, name = s"$name.enter")

  // enter is up iff a new batch of processes can enter the barrier
  // each entering process but the last is stalled by wait
  private[this] var result = e

  /** Wait until all `n` processes have called sync */
  def sync(id: Int, t: T): T =
    if (shared) {
      enter.acquire()
      result = op(result, t) // add the last contribution
      // the last process arrives
      if (waiting == n - 1) // the last process arrives
        try { result }
        finally { wait.release() } // everyone can proceed (but cannot re-enter)
      // a process arrives that isn't the last
      else {
        waiting += 1
        enter.release() // another can enter
        wait.acquire() // the caller waits
        waiting -= 1 // after the wait finishes
        try { return result } // return a result
        finally {
          if (waiting == 0) { result = e; enter.release() }
          else wait.release()
        }
      }
    } else {
      return op(result, t)
    }

  /** This barrier doesn't need to know about the thread id */
  def sync(t: T): T = sync(0, t)
}

class BarrierUnit(n: Int, name: String = "")
    extends LinearBarrier[Unit](n, (), (_: Unit, _: Unit) => (), name) {
  def sync(): Unit = sync(0, ())
}

object LinearBarrier {
  def apply(n: Int, name: String = "") =
    new BarrierUnit(n)
}
