package io.threadcso.semaphore

import java.util.concurrent.atomic._
import java.util.concurrent.locks.LockSupport
import io.threadcso.basis.{Identity, NameGenerator}

/** A fast boolean semaphore implemented with a nonblocking queue.
  *
  * The object `parent` (by default the semaphore itself) is used by the
  * debugger when reporting the state of waiting processes.
  *
  * The object `parent` is shown by the debugger when doing stack backtraces.
  *
  * The value `spin` is the number of attempts the semaphore's inbuilt spinlock
  * will make to seize the semaphore before a call to `acquire` or `acquireFast`
  * causes the caller to be descheduled. The point of trying with a spinlock
  * first is that the performance-hit of descheduling can be very high. We
  * assume here that more than one processor is available: set `spin` to `0` or
  * `1` otherwise.
  *
  * The `fair` parameter is ignored.
  *
  * @todo
  *   Make the internal spinlock ''adaptive''
  */
class BooleanSemaphore(
    val available: Boolean,
    private var name: String,
    val fair: Boolean,
    val parent: AnyRef,
    val spin: Int = 5
) extends Semaphore {
  name = BooleanSemaphore.genName(name)
  private[this] val _owner = new AtomicReference(
    if (available) null else Thread.currentThread
  )
  private[this] val _waiting: Queue[Thread] = new LockFreeQueue[Thread]
  private[this] val _behalf = if (parent == null) this else parent
  @volatile
  private[this] var _cancelled = false

  /** "Interrupt" each thread waiting to acquire this semaphore, causing the
    * `acquire` to terminate, with `cancelled()` true.
    */
  override def cancel() = {
    _cancelled = true
    var n = _waiting.length
    while (n > 0) { n -= 1; release() }
  }

  override def toString: String =
    s"""$name: ${if (_owner.get == null) "available"
      else s"owner: ${_owner.get.identity}"} ${if (_cancelled) "[cancelled]"
      else ""} [${_waiting.length} ${_waiting.elements
        .map(_.identity)
        .mkString(", ")}]"""

  /** Return true iff the semaphore has been acquired with a short busy wait.
    * This avoids entering the scheduler (which is costly) when a rival that
    * owns the semaphore is not going to stay long.
    */
  @inline private def acquireFast(owner: Thread): Boolean = {
    var n = 0
    while (n < spin) {
      if (_owner.compareAndSet(null, owner)) return true
      n += 1
    }
    false
  }

  /** Throw an `InterruptedException`
    */
  @inline private[this] def throwInterrupt() = throw new InterruptedException(
    s"Semaphore Interrupted: ${this} for ${_behalf}"
  )

  /** Wait until the semaphore is unlocked. If the acquiring thread is cancelled
    * during the wait or the semaphore's `cancel()` method has been called, then
    * rethrow the cancel.
    */
  def acquire(): Unit = {
    if (_cancelled) return // the Semaphore has been cancelled
    val current = Thread.currentThread
    if (acquireFast(current)) return
    _waiting.enqueue(current)
    // Deschedule the current thread if it's not at the front of the queue or if the semaphore is locked
    while (
      (_waiting.peek() != current || !_owner.compareAndSet(null, current))
    ) {
      LockSupport.park(_behalf)
    }

    // This thread is at the front of the queue and the lock has been set, so remove the front of the queue
    _waiting.removeFirst()
    ()
  }

  /** true if the semaphore has been cancelled during an `acquire`.
    */
  override def cancelled(): Boolean = _cancelled

  /** Unlock the semaphore and permit an `acquire`ing process to proceed. */
  def release(): Unit = {
    _owner.set(null)
    val waiter = _waiting.peek()
    LockSupport.unpark(waiter)
  }

  /** Wait until the semaphore becomes available (returning true) or until the
    * specified timeout has elapsed (returning false). Unlike
    * `java.util.concurrent.Semaphore` an invoking thread that needs to wait
    * joins the queue of waiting threads
    * -- threads with shorter timeouts are not prioritised over those with
    * longer deadlines or no deadline. If the acquiring thread is cancelled
    * during the wait or the semaphore's `cancel()` method has been called, then
    * the `cancelled()` method will return true.
    */

  override def tryAcquire(timeoutNS: Long): Boolean = {
    if (_cancelled) return false
    val current = Thread.currentThread
    if (acquireFast(current)) return true
    if (_cancelled) return false
    val deadline = timeoutNS + System.nanoTime
    var left = timeoutNS
    var trying = true
    var outcome = false
    _waiting.enqueueFirst(current)
    while (trying) {
      if (_waiting.peek() == current && _owner.compareAndSet(null, current)) {
        trying = false; outcome = true
      } else {
        LockSupport.parkNanos(_behalf, left)
        left = deadline - System.nanoTime
        trying = left > 0
      }
    }

    _waiting.removeFirst()
    outcome
  }

  override def getWaiting: collection.Seq[Thread] = _waiting.elements

  override def remaining: Int = if (_owner.get != null) 0 else 1

}

/** Semaphore factory.
  */
object BooleanSemaphore extends NameGenerator("BooleanSemaphore") {

  /** Construct a boolean semaphore.
    *
    * @param available
    *   whether the semaphore is initially available
    * @param name
    *   the name of the semaphore
    * @param fair
    *   whether a first-come, first-served semaphore is to be returned @see
    *   [[io.threadcso.semaphore.jdk.BooleanSemaphore]]
    * @param parent
    *   if not `fair` then the debugger shows the parent in stack backtraces; if
    *   `fair` then the debugger shows the semaphore itself iff `parent!=null`
    * @param spin
    *   if not `fair` then this is the threshold number of times an attempt is
    *   made to acquire the semaphore using a spinlock before the acquiring
    *   thread is descheduled.
    * @return
    *   a boolean semaphore
    */
  def apply(
      available: Boolean = false,
      name: String = null,
      fair: Boolean = false,
      parent: AnyRef = null,
      spin: Int = 5
  ): Semaphore =
    if (fair)
      new io.threadcso.semaphore.jdk.BooleanSemaphore(
        available,
        name,
        true,
        parent != null
      )
    else
      new BooleanSemaphore(available, name, false, parent, spin)
}
