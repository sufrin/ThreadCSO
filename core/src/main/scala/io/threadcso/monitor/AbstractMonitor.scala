package io.threadcso.monitor

import java.util.concurrent.locks.{Condition, ReentrantLock}

import io.threadcso.debug.REGISTRY.Debuggable

/** Abstract precursor of monitor implementations that use
  * `jdk.util.concurrent.lock` classes. If `fair` is true then the lock is
  * first-come first-served. Under heavy contention this can be useful, but it
  * appears to impose a penalty of (around) a factor of 10 in the elapsed time
  * to acquisition of the lock.
  *
  * If a monitor is registered with the debugger its state will be shown in a
  * debugger snapshot if its lock is owned (or awaited) at the time of the
  * snapshot. This can be helpful for ''in-extremis'' debugging.
  *
  * Objects that use one of these monitor as a source of `Condition`s in their
  * implementation must themselves register with the debugger and must
  * themselves implement the debugger's `hasState/showState/getWaiting/`
  * protocol.
  *
  * The following is an implementation of a one-slot buffer using two condition
  * queues so that multiple producer (consumer) processes can invoke put (get).
  *
  * {{{
  * class MonitorSlot[T] extends Slot[T] {
  * private [this] val monitor = new Monitor
  * private [this] var value: T            = _
  * private [this] var empty               = true
  * private [this] val isEmpty, isNonEmpty = monitor.newCondition
  *
  * def put(v: T) = monitor withLock
  * { while (!empty) isEmpty.await()
  * value = v; empty = false
  * isNonEmpty.signal()
  * }
  *
  * def get: T = monitor withLock
  * { while (empty) isNonEmpty.await()
  * val result = value; empty = true
  * isEmpty.signal()
  * return result
  * }
  * }
  * }}}
  *
  * The following is an implementation of a one-slot buffer using just a single
  * queue. Multiple producer (consumer) processes can still invoke put (get),
  * but note the use of the (less efficient under load) `signalAll` method.
  * {{{
  * class MonitorSlot2[T] extends Slot[T] {
  * private [this] val monitor = new Monitor
  * private [this] var value: T = _
  * private [this] var empty    = true
  * private [this] val waiting  = monitor.newCondition
  *
  * def put(v: T) = monitor withLock
  * { while (!empty) waiting.await()
  * value = v; empty = false
  * waiting.signalAll()
  * }
  *
  * def get: T = monitor withLock
  * { while (empty) waiting.await()
  * val result = value; empty = true
  * waiting.signalAll()
  * return result
  * }
  * }
  * }}}
  */
class AbstractMonitor(fair: Boolean, private[this] var name: String = null)
    extends Debuggable {

  /** The reentrant lock used by this monitor */
  private[this] val lock = new XReentrantLock(fair)

  if (name == null) name = s"AbstractMonitor($fair)@$hashCode"

  /* State description for debugger */

  /** The threads waiting for this lock. */
  override def getWaiting: collection.Seq[Thread] = lock.getWaiting

  /** The current state of the lock */
  override def toString: String = {
    val o = lock.theOwner
    val w = getWaiting
    if (o == null && w.isEmpty)
      s"Monitor($fair, $name)"
    else
      s"Monitor($fair, $name) owned by $o awaited by $w"
  }

  /** Output the state of an owned and awaited monitor */
  override def showState(out: java.io.PrintWriter): Unit = {
    val o = lock.theOwner
    val w = getWaiting
    if (o == null && w.isEmpty)
      ()
    else
      out.print(s"Monitor($fair, $name) owned by $o awaited by $w")
  }

  /** Returns true iff it's owned or somebody's waiting (unlikely to be the
    * latter without the former). Used only in the debugger to detect
    * interesting monitor.
    */
  override def hasState: Boolean = lock.theOwner != null || getWaiting.nonEmpty

  /** An exuberant rentrant lock that will divulge threads waiting on a
    * particular condition. Useful in the debugger.
    */
  private class XReentrantLock(fair: Boolean) extends ReentrantLock(fair) {
    @inline def getWaiting(condition: Condition): collection.Seq[Thread] = { // import scala.collection.convert.WrapAsScala.collectionAsScalaIterable
      import scala.jdk.CollectionConverters._ // collection.JavaConverters._

      if (tryLock)
        try {
          super.getWaitingThreads(condition).asScala.toList
        } // collectionAsScalaIterable(super.getWaitingThreads(condition)).toList }
        finally { unlock() }
      else
        List()
    }
    @inline def getWaiting
        : collection.Seq[Thread] = { // import scala.collection.convert.WrapAsScala.collectionAsScalaIterable
      import scala.jdk.CollectionConverters._
      super.getQueuedThreads.asScala.toList
    }
    @inline def theOwner: Thread = super.getOwner

  }

  // @inline def newCondition: Condition = lock.newCondition
  @inline def newCondition = new LockSupportCondition(lock)

  /** Evaluate `body` with the lock set; and then unset the lock. The lock is
    * unset even if `body` throws an exception.
    */
  @inline def withLock[T](body: => T): T = {
    lock.lock()
    try { body }
    finally { lock.unlock() }
  }

  /** Try the lock for up to `timeoutNS`, and if it lock then evaluate `body`
    * else evaluate `otherwise`
    */
  @inline def tryLockFor[T](timeoutNS: Long)(body: => T)(otherwise: => T): T = {
    if (
      lock.tryLock || lock.tryLock(
        timeoutNS,
        java.util.concurrent.TimeUnit.NANOSECONDS
      )
    )
      try { body }
      finally { lock.unlock() }
    else otherwise
  }

  /** ''Approximate'' sequence of threads awaiting `condition`. Yields an empty
    * sequence if this monitor is owned by a thread other than the caller when
    * it is called. The approximation is adequate only for use by a debugger.
    */
  @inline def getWaiting(condition: Condition): collection.Seq[Thread] =
    lock.getWaiting(condition)

  /** ''Approximate'' answer to the question ''are any threads awaiting''
    * `condition`? Yields false if the monitor is owned by a thread other than
    * the caller when it is called. The approximation is adequate only for use
    * by a debugger.
    */
  @inline def waitingFor(condition: Condition): Boolean =
    if (lock.tryLock)
      try { lock.hasWaiters(condition) }
      finally { lock.unlock() }
    else
      false

}
