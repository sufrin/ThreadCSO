package io.threadcso.channel

import java.util.concurrent.atomic.{AtomicReference => Atomic}

import io.threadcso.basis._
import io.threadcso.lock.JavaLock
import io.threadcso.nanoTime

/** Static generator(s) for shared synchronized channel
  *
  * @see
  *   [[io.threadcso.channel.N2N]]
  * @param writers
  *   expected number of writers
  * @param readers
  *   expected number of readers
  * @param name
  *   name of the buffer (for the debugger)
  * @tparam T
  *   type of value transmitted by the buffer
  */

object N2N extends NameGenerator("N2N") {
  def apply[T](
      writers: Int = 0,
      readers: Int = 0,
      name: String = newName(),
      fairOut: Boolean = false,
      fairIn: Boolean = false
  ): N2N[T] =
    new N2N(writers, readers, name, fairOut, fairIn)

  type ManyOne[T] = SharedChan[T]
  type OneMany[T] = SharedChan[T]
  type ManyMany[T] = SharedChan[T]

  def ManyOne[T](
      writers: Int = 0,
      name: String = newName("ManyOne")
  ): ManyOne[T] = apply[T](writers, 1, name)
  def OneMany[T](
      readers: Int = 0,
      name: String = newName("OneMany")
  ): OneMany[T] = apply[T](1, readers, name)
  def ManyMany[T](name: String = newName("ManyMany")): ManyMany[T] =
    apply[T](0, 0, name)
}

/** Synchronized shared channel to support communication between `writers`
  * writers and `readers` readers. Closes completely when `closeOut` has been
  * invoked `writers` times, or `closeIn` has been invoked `readers` times. If
  * either `writers` or `readers` is non-positive, then the channel can be
  * closed an unbounded number of times in the associated direction.
  *
  * There is no check for writer-overtaking in an `N2N(1,_)` or for
  * reader-overtaking in an `N2N(_, 1)`. Both ends of an `N2N` behave as if they
  * were shared.
  *
  * The `fairOut` and `fairIn` parameters control whether processes are
  * FIFO-queued to use the output/input side of the underlying synchronous
  * channel when there is heavy contention. They both default to false -- for
  * efficiency when there is little contention.
  *
  * @param writers
  *   expected number of writers
  * @param readers
  *   expected number of readers
  * @param name
  *   name of the buffer (for the debugger)
  * @tparam T
  *   type of value transmitted by the buffer
  */
class N2N[T](
    writers: Int,
    readers: Int,
    name: String,
    fairOut: Boolean,
    fairIn: Boolean
) extends OneOne[T](name)
    with SharedChan[T] {
  private[this] val ws = new java.util.concurrent.atomic.AtomicInteger(writers)
  private[this] val rs = new java.util.concurrent.atomic.AtomicInteger(readers)
  private[this] val wm = new JavaLock(fairOut)
  private[this] val rm = new JavaLock(fairIn)

  override def closeOut(): Unit = if (ws.decrementAndGet == 0) close()

  override def closeIn(): Unit = if (rs.decrementAndGet == 0) close()

  override def !(value: T): Unit = wm withLock { super.!(value) }

  override def writeBefore(nsWait: Long)(value: T): Boolean = {
    val deadline = nanoTime + nsWait
    wm.tryLockFor(nsWait) {
      val remaining = deadline - nanoTime
      if (remaining > 0)
        super.writeBefore(remaining)(value)
      else
        false
    } { false }
  }

  override def ?(): T = rm withLock { super.?() }

  override def ?[U](f: T => U): U = rm withLock { super.?(f) }

  override def ??[U](f: T => U): U = rm withLock { super.??(f) }

  override def readBefore(nsWait: Long): Option[T] = {
    val deadline = nanoTime + nsWait
    rm.tryLockFor(nsWait) {
      val remaining = deadline - nanoTime
      if (remaining > 0)
        super.readBefore(remaining)
      else
        None
    } { None }
  }

  /** Return an approximation to the set of threads waiting for this channel.
    * Intended to be invoked (from the debugger) only when system is quiescing
    * or deadlocked.
    */
  override def getWaiting: collection.Seq[Thread] =
    super.getWaiting ++
      wm.getWaiting ++
      rm.getWaiting

  /** Not thread-safe: best invoked only when system is quiescing (when the
    * debugger is invoked).
    */
  override def showState(out: java.io.PrintWriter): Unit = {
    val ww = wm.numWaiting
    val rw = rm.numWaiting
    super.showState(out)
    out.print(s"\n\t(${ws.get} writers and ${rs.get} readers remaining)")
    if (rw > 0) out.print(s"""\n\tInPort queue: [${rm.getWaiting
        .map(_.identity)
        .mkString(", ")}]""")
    if (ww > 0) out.print(s"""\n\tOutPort queue: [${wm.getWaiting
        .map(_.identity)
        .mkString(", ")}]""")
  }

  override def nameGenerator: NameGenerator = N2N
}
