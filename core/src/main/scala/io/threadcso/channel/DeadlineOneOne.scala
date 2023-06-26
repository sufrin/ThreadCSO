package io.threadcso.channel

/** A OneOne-like channel, including a writeBefore operation. */
class DeadlineOneOne[T](name: String = "") extends SyncChan[T] {

  /** The current value being sent; valid if full = true. */
  private[this] var buffer: T = _

  /** Is the current value in buffer full. */
  private[this] var full = false

  // ======== Closing of transport

  /** Is the channel closed? */
  private[this] var closed = false

  override def close() = synchronized { closed = true; notifyAll }

  override def closeIn(): Unit = close()

  override def closeOut() = close()

  /** Check the channel is open. */
  @inline private[this] def checkOpen =
    if (closed) throw new Closed(name)

  // ======== Main operations

  override def !(value: T) = synchronized {
    checkOpen; assert(!full)
    buffer = value; full = true; notify() // wake the reader
    while (full) wait() // wait for the reader
  }

  override def ?(): T = synchronized {
    checkOpen
    while (!full && !closed) wait() // wait for the writer to fill the buffer
    if (!full) throw new Closed(name)
    full = false; notify() // wake the sender
    buffer // Note: we retain the lock so no race condition on buffer
  }

  override def writeBefore(nsWait: Long)(value: T): Boolean = synchronized {
    checkOpen; assert(!full)
    buffer = value; full = true; notify() // wake the reader
    val deadline = System.nanoTime + nsWait // timeout time
    while (full && deadline - System.nanoTime > 0) {
      val delay = deadline - System.nanoTime
      // wait for the reader or for delay ns
      if (delay > 0) wait(delay / 1000000, (delay % 1000000).toInt)
    }
    if (full) { // timeout
      full = false; false
    } else true
  }

  override def ?[U](f: T => U): U = ???
  override def ??[U](f: T => U): U = ???
  override def canInput: Boolean = ???
  override def inPortState: io.threadcso.channel.PortState = ???
  override def readBefore(ns: Long): Option[T] = ???

  // Members declared in io.threadcso.basis.Named
  override def nameGenerator: io.threadcso.basis.NameGenerator = ???

  // Members declared in io.threadcso.channel.OutPort
  override def canOutput: Boolean = ???
  override def outPortState: io.threadcso.channel.PortState = ???

}
