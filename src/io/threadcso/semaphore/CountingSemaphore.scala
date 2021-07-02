package io.threadcso.semaphore

import java.util.concurrent.atomic._
import java.util.concurrent.locks.LockSupport
import io.threadcso.basis.{Identity, NameGenerator}

/**
        A counting semaphore implemented with a nonblocking queue. In principle
        the semaphore manages a finite collection of (identical) tokens.

        The object `parent` (by default the semaphore itself) is
        used by the debugger when reporting the state of waiting
        processes.

        The value of `available` may be negative, in which case it represents a deficit of tokens.

        The value `spin` is the number of attempts the semaphore's local spinlock will make to acquire a token
        before a call to `acquire` or `acquireFast` causes the caller to be descheduled. The point of
        trying with a spinlock first is that the performance-hit of descheduling can be very high. We assume here
        that more than one processor is available: set `spin` to `0`  or `1` otherwise.

        The `fair` parameter is ignored.

        @todo Make the internal spinlock ''adaptive''

  */
class CountingSemaphore(val available: Int, var name: String, val fair: Boolean, val parent: AnyRef, val spin: Int=5)
  extends Semaphore
{ name = CountingSemaphore.genName(name)
  private [this] val _count = new AtomicInteger(available)
  private [this] val _waiting: Queue[Thread] = new LockFreeQueue[Thread]
  private [this] val _behalf = if (parent==null) this else parent
  @volatile
  private [this] var _cancelled = false

  override def toString: String =
    s"""$name: ${_count.get} available [${_waiting.length} ${_waiting.elements.map(_.identity).mkString(", ")}]"""

  /**
    * Interrupt each thread waiting to acquire this semaphore, and
    * cause it to throw an `InterruptedException`. Mark the semaphore
    * itself cancelled
    */
  override def cancel() =
  { _cancelled = true
    var n = _waiting.length
    while (n>0) { n -= 1; release() }
    //_count.set(0)
    //_waiting.clear()
  }

  /** Return true iff a token has been acquired with a short busy wait. This avoids entering
    * the scheduler (which is costly) when a rival that owns the semaphore is not going to stay long.
    * */
  @inline private def acquireFast(): Boolean =
  { var n = 0
    while (n < spin) {
      if (atomicDec) return true
      n += 1
    }
    false
  }

  /**
    * Throw an `InterruptedException`
    */
  @inline private [this] def throwInterrupt() = throw new InterruptedException(s"Semaphore Interrupted: ${this} for ${_behalf}")

  /**
    *   Wait until a token is (or becomes) available.
    *   If the acquiring thread is cancelled during the wait or the semaphore's `cancel()` method has been called, then
    *   rethrow the cancel.
    */
  def acquire(): Unit = {
    if (_cancelled) return
    if (acquireFast()) return
    val current = Thread.currentThread
    _waiting.enqueue(current)
    // Deschedule the current thread if it's not at the front of the queue or if the semaphore is locked
    while ((_waiting.peek() != current || !atomicDec)) {
      LockSupport.park(_behalf)
    }

    // This thread is at the front of the queue
    _waiting.removeFirst()

    // pass the baton
    if (_count.get > 0) signal
    ()
  }

  @inline private [this] def atomicDec: Boolean = _count.getAndUpdate({n => if (n>0) n-1 else n})>0

  @inline private [this] def signal: Unit = LockSupport.unpark(_waiting.peek())

  /**
    * Add a token to the collection, then permit an `acquire`ing process to
    * proceed if there is at least one token available.
    */
  def release(): Unit =
  {   if (_count.incrementAndGet > 0) signal
  }

  /**
    * Reinitialize the semaphore to its as-new state.
    */

  def reInitialize(): Unit =
  { assert (_waiting.length==0, s"$name should have no waiting threads.")
    _count.set(0)
    _waiting.clear()
  }

  /**
    * Returns true if the semaphore was cancelled
    */
  override def cancelled(): Boolean = _cancelled


  /**
    * Wait until a token becomes available (returning true) or until the specified timeout has elapsed (returning false).
    * Unlike `java.util.concurrent.Semaphore` an invoking thread that needs to wait joins the queue of waiting threads
    * -- threads with shorter timeouts are not prioritised over those with longer deadlines or no
    * deadline.
    * If the acquiring thread is cancelled during the wait or the semaphore's `cancel()` method has been called,
    * then mark the semaphore as having been cancelled
    */

  override
  def tryAcquire(timeoutNS: Long) : Boolean =
  { if (_cancelled) return false
    if (acquireFast()) return true
    if (Thread.interrupted) { _cancelled=true; return false }
    val current  = Thread.currentThread
    val deadline = timeoutNS+System.nanoTime
    var left     = timeoutNS
    var trying   = true
    var outcome  = false
    _waiting.enqueueFirst(current)
    while (trying && ! _cancelled)
    {  if (_waiting.peek() == current && atomicDec)
       { trying = false; outcome = true }
       else
       { LockSupport.parkNanos(_behalf, left)
         left = deadline - System.nanoTime
         trying = left>0
       }
    }

    _waiting.removeFirst()
    if (_count.get>0) signal
    outcome
  }

  override def getWaiting: collection.Seq[Thread] = _waiting.elements

  override def remaining: Int = _count.get()

}

/**
  * Factory for Counting semaphores
  */
object CountingSemaphore extends NameGenerator("CountingSemaphore")
{
  /** Construct a boolean semaphore.
    *
    * @param available initial number of available ''tokens''
    * @param name the name of the semaphore
    * @param fair whether a first-come, first-served semaphore is to be returned @see [[io.threadcso.semaphore.jdk.CountingSemaphore]]
    * @param parent if not `fair` then the debugger shows the parent in stack backtraces;
    *               if `fair` then the debugger shows the semaphore itself iff `parent!=null`
    * @param spin   if not `fair` then this is the threshold number of times an attempt is made to acquire the semaphore using
    *               a spinlock before the acquiring thread is descheduled.
    * @return a boolean semaphore
    */
  def apply(available: Int=0, name: String = null, fair: Boolean = false, parent: AnyRef = null, spin: Int=5): Semaphore =
    if (fair)
            new io.threadcso.semaphore.jdk.CountingSemaphore(available, name, true, parent!=null)
      else
            new CountingSemaphore(available, name, false, parent, spin)
}







