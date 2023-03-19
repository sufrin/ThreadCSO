package io.threadcso.semaphore.jdk

import io.threadcso.basis.{Identity, NameGenerator}
import io.threadcso.debug.REGISTRY.Debuggable
import io.threadcso.semaphore.Semaphore
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}

/** A counting semaphore which maintains a virtual/conceptual set of permits.
  * Each `acquire` waits if necessary until a permit is available, and then
  * removes it from the set. Each `release` adds a permit, potentially causing a
  * waiting `acquire` to return.
  *
  * If the semaphore is `fair` the semaphore will (under contention) guarantee
  * first-in first-out acquisition of permits (by default it is not fair).
  * Fairness can impose considerable delays.
  *
  * No actual permit objects are involved; the semaphore just keeps a count of
  * the number `available`, ''which may initially be negative.''
  *
  * A thread that `release`s a permit ''need not have acquired one previously''.
  *
  * This implementation delegates to `java.util.concurrent.Semaphore`.
  *
  * A `CountingSemaphore` can be registered with the debugger.
  *
  * @deprecated
  *   Use [[io.threadcso.semaphore.CountingSemaphore]] instead unless you
  *   absolutely need the fairness parameter.
  */
class CountingSemaphore(
    available: Int,
    var name: String = null,
    fair: Boolean = false,
    _register: Boolean = false
) extends Debuggable
    with Semaphore {
  import io.threadcso.debug.REGISTRY.showThreads
  name = CountingSemaphore.genName(name)
  private[this] val _cancelled = new AtomicBoolean(false)
  if (_register) this.register()
  private[this] class Sem
      extends java.util.concurrent.Semaphore(available, fair) {
    def waiting: List[Thread] = {
      var l: List[Thread] = List()
      val it = getQueuedThreads.iterator()
      while (it.hasNext) l = it.next :: l
      l
    }

    def interrupt(): Unit = {
      val it = getQueuedThreads.iterator()
      while (it.hasNext) it.next.interrupt()
    }
  }
  private[this] val s = new Sem
  override def toString = s"""$name(${s.availablePermits}/$available) ${if (
      cancelled()
    ) "[cancelled]"
    else ""} [waiting: ${s.waiting.map(_.identity).mkString(",")}]"""

  // TODO: Not really expecting an Interrupt, but this is belt-and-braces
  def acquire(): Unit = try { s.acquire() }
  catch { case _: InterruptedException => _cancelled.set(true) }

  def release(): Unit = s.release()

  override def cancel(): Unit = {
    _cancelled.set(true); s.release(2 * s.getQueueLength())
  } // avoid interrupting

  override def cancelled(): Boolean = _cancelled.get()

  override def tryAcquire(wait: Long): Boolean =
    if (cancelled()) false
    else s.tryAcquire(wait, java.util.concurrent.TimeUnit.NANOSECONDS)

  //////// debugger interface
  override def getWaiting: Seq[Thread] = s.waiting
  override def showState(out: java.io.PrintWriter): Unit = {
    out.print(s"CSEMA   $name ")
    showThreads(out, " waiting: ", getWaiting)
  }

  def reInitialize(): Unit = {
    assert(!s.hasQueuedThreads, s"Waiting threads: ${s.toString}")
    s.drainPermits()
  }

}

object CountingSemaphore extends NameGenerator("CountingSemaphore") {
  def apply(
      available: Int,
      name: String = null,
      fair: Boolean = false,
      register: Boolean = false
  ): CountingSemaphore =
    new CountingSemaphore(available, name, fair, register)
}
