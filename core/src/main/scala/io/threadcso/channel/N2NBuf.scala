package io.threadcso.channel

import java.util.concurrent.TimeUnit.NANOSECONDS

import io.threadcso.basis.{NameGenerator, Nanoseconds}

import java.util.concurrent.atomic._

import scala.annotation._
import scala.annotation.elidable._

/** Static generator for `N2NBuf`
  *
  * @see
  *   [[io.threadcso.channel.N2NBuf]]
  * @param size
  *   buffer is bounded by `size` if it is strictly positive, else unbounded
  * @param writers
  *   expected number of readers
  * @param readers
  *   expected number of writers
  * @param name
  *   name of the buffer (for the debugger)
  * @tparam T
  *   type of value transmitted by the buffer
  */
object N2NBuf extends NameGenerator("N2NBuf") {
  def apply[T](
      size: Int = 0,
      writers: Int = 0,
      readers: Int = 0,
      name: String = newName()
  ): N2NBuf[T] =
    new N2NBuf[T](size, writers, readers, name)
}

/** A general-purpose shared buffered channel, bounded by `size` if it is
  * positive, and unbounded otherwise.
  *
  * The buffer can be shared by any number of readers and writers.
  *
  * It closes completely when `closeIn` has been invoked `readers` times, or
  * `closeOut` has been invoked `writers` times and the buffer has been emptied.
  *
  * If either `writers` or `readers` is non-positive, then the channel can be
  * closed an unbounded number of times in the associated direction.
  *
  * There is no attempt to ensure fairness between sharing writers or sharing
  * readers.
  *
  * @param size
  *   buffer is bounded by `size` if it is strictly positive, else unbounded
  * @param writers
  *   expected number of readers
  * @param readers
  *   expected number of writers
  * @param name_
  *   name of the buffer (for the debugger)
  * @tparam T
  *   type of value transmitted by the buffer
  */
class N2NBuf[T](size: Int, writers: Int, readers: Int, name_ : String)
    extends SharedChan[T] {

  /** Whether the input (output) port can no longer be used */
  private[this] val inputClosed, outputClosed =
    new java.util.concurrent.atomic.AtomicBoolean(false)

  /** Remaining writers */
  private[this] val ws = new java.util.concurrent.atomic.AtomicInteger(writers)

  /** Remaining readers */
  private[this] val rs = new java.util.concurrent.atomic.AtomicInteger(readers)

  def inPortState: PortState =
    if (inputClosed.get) CLOSEDSTATE
    else if (isEmpty) UNKNOWNSTATE
    else READYSTATE

  def outPortState: PortState =
    if (outputClosed.get) CLOSEDSTATE
    else if (isFull) UNKNOWNSTATE
    else READYSTATE

  /** Count oof the number of finished reads (writes) */
  // @elidable (FINEST)
  val reads, writes = new AtomicLong(0)

  /** Increment count of finished reads */
  @elidable(FINEST) def finishedRead = reads.incrementAndGet()

  /** Increment count of finished writes */
  @elidable(FINEST) def finishedWrite = writes.incrementAndGet

  /** Intelligible information about finished reads and writes */
  @elidable(FINEST) def finishedRW =
    s"(READ ${reads.get}, WRITTEN ${writes.get})"

  def nameGenerator: NameGenerator = N2NBuf

  setName(name_)
  this.register()
  // outPortEvent(READYSTATE)  // Initialization delayed in non-alt variant
  // inPortEvent(UNKNOWNSTATE) // ditto

  /** Queue of buffered data */
  private[this] val queue = new BlockingQueue[T](size, this, name)

  @inline private[this] def state(port: String, closed: Boolean) =
    if (closed) s"$port (CLOSED),  " else ""

  override def toString: String = {
    s"""CHANNEL ${this.name}: ${nameGenerator.kind} ${state(
        "OutPort",
        outputClosed.get
      )} """ +
      s"""${state(
          "InPort",
          inputClosed.get
        )}(writers=${ws.get}, readers=${rs.get}) """ +
      s"""size=${size}, length=${queue.size}, remainingCapacity=${queue.remainingCapacity})""" + finishedRW
  }

  override def showState(out: java.io.PrintWriter) = {
    out.print(toString)
  }

  def close() = { // Set state variables
    outputClosed.set(true)
    inputClosed.set(true)
    outPortEvent(CLOSEDSTATE)
    inPortEvent(CLOSEDSTATE)
    queue.clear()
    this.unregister()
  }

  def closeOut() =
    if (ws.decrementAndGet == 0) {
      outputClosed.set(true)
      if (isEmpty) close()
    }

  def canInput =
    !(outputClosed.get && isEmpty)

  def closeIn() =
    if (rs.decrementAndGet == 0) {
      inputClosed.set(true)
      outPortEvent(CLOSEDSTATE)
      close()
    }

  def canOutput = !inputClosed.get

  @inline def isEmpty = queue.isEmpty

  @inline def isFull = queue.remainingCapacity > 0

  def ?(): T = {
    if (inputClosed.get) throw new Closed(name)
    if (outputClosed.get && isEmpty) {
      close(); /* *** */
      throw new Closed(name)
    }
    try {
      outPortEvent(READYSTATE)
      val r = queue.take()
      finishedRead
      r
    } catch // TODO: unnecessary because handled by queue
      {
        case _: InterruptedException => {
          throw new Closed(name)
        }
      }
  }

  def ?[U](f: T => U): U = f(?())

  def ??[U](f: T => U): U = f(?())

  // TODO: TEST INTERACTION WITH Alt
  def readBefore(nsWait: Nanoseconds): Option[T] = {
    if (inputClosed.get) throw new Closed(name)
    if (outputClosed.get && isEmpty) {
      close(); /* *** */
      throw new Closed(name)
    }
    outPortEvent(READYSTATE)
    queue.takeBefore(nsWait)
  }

  def !(value: T): Unit = {
    if (outputClosed.get || inputClosed.get) throw new Closed(name)
    try {
      queue.put(value)
      inPortEvent(READYSTATE)
      finishedWrite
    } catch // TODO: unnecessary because handled by queue
      {
        case _: InterruptedException => {
          throw new Closed(name)
        }
      }
    if (inputClosed.get)
      throw new Closed(name) // Because there is no chance of it being read
  }

  // TODO: REVIEW INTERACTION WITH Alt
  def writeBefore(nsWait: Long)(value: T): Boolean = {
    if (outputClosed.get || inputClosed.get) throw new Closed(name)
    val ok = queue.putBefore(nsWait, value)
    ok
  }

}
