package io.threadcso.semaphore

import java.util.concurrent.atomic._
import java.util.concurrent.locks.LockSupport

import io.threadcso.basis.{Identity, NameGenerator}

/** A synchronization object that supports threads waiting until a fixed number
  * of operations being performed in other threads have been completed.
  *
  * A `Latch` is initialized with a given count. The `await` methods block until
  * the current count reaches zero through `decrement` invocations, after which
  * all waiting threads are allowed to proceed and any subsequent invocations of
  * `await` return immediately. This happens exactly once per latch: its count
  * cannot be reset.
  *
  * If the latch is strict then any `decrement` calls subsequent to the one that
  * makes the count zero will raise an `IllegalStateException`.
  */

class Latch(count: Int, var name: String = null, strict: Boolean = true) {
  name = Latch.genName(name)
  private[this] val _count = new AtomicInteger(count)
  private[this] val _waiting =
    new java.util.concurrent.ConcurrentLinkedQueue[Thread]

  override def toString: String =
    s"$name: ${_count}/$count strict=$strict [${_waitingThreads.map(_.identity)}]"

  /** Peek at the waiting queue: for debugger support
    */
  private[threadcso] def _waitingThreads: collection.Seq[Thread] = {
    val ts = Array.ofDim[Thread](0)
    for (t <- _waiting.toArray(ts)) yield t
  }

  /** Causes the current thread to wait until the latch has counted down to
    * zero.
    */
  def await(): Unit = {
    if (_count.get <= 0) return
    val current = Thread.currentThread
    _waiting.add(current)
    while (_count.get > 0) LockSupport.park(this)
    ()
  }

  /** Causes the current thread to wait until the latch has counted down to zero
    * (returning true) or until the specified timeout has elapsed (returning
    * false).
    */
  def tryAwait(timeoutNS: Long): Boolean = {
    if (_count.get <= 0) return true

    val current = Thread.currentThread
    val deadline = timeoutNS + System.nanoTime
    var waiting, outcome = true
    _waiting.add(current)

    while (waiting) {
      if (_count.get <= 0) { waiting = false }
      else {
        val left = deadline - System.nanoTime
        if (left <= 0L) { outcome = false; waiting = false }
        else {
          LockSupport.parkNanos(this, left)
          // ASSERT: deadline expired || unparked
          if (deadline < System.nanoTime) { outcome = false; waiting = false }
        }
      }
    }
    _waiting.remove(current)
    return outcome
  }

  /** Decrements the count, allowing all awaiting threads to proceed if the
    * count thereby reaches zero.
    */
  def decrement(): Unit = {
    val remaining = _count.decrementAndGet
    if (remaining == 0)
      while (!_waiting.isEmpty) LockSupport.unpark(_waiting.poll)
    else if (strict && remaining < 0)
      throw new IllegalStateException("Latch already terminated: " + toString)
  }

  /** Return the current value of the count.
    */
  def getCount: Int = _count.get

}

object Latch extends NameGenerator("Latch") {
  def apply(count: Int, name: String = null) = new Latch(count, name)
}
