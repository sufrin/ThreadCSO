package io.threadcso.semaphore

/** A lock-free queue that permits barging. Implemented as a facade for
  * `java.util.concurrent.ConcurrentLinkedDeque`. It has a 40% efficiency
  * overhead relative to `LockFreeQueue`.
  *
  * @todo
  *   reimplement as a simpler variant of
  *   `java.util.concurrent.ConcurrentLinkedDeque`.
  */
sealed private[semaphore] class LockFreeDeQueue[T] extends Queue[T] {
  private[this] val _delegate =
    new java.util.concurrent.ConcurrentLinkedDeque[T]

  def length: Int = _delegate.size

  def enqueue(value: T): Unit = {
    _delegate.addLast(value)
    ()
  }

  def enqueueFirst(value: T): Unit = {
    _delegate.addFirst(value)
    ()
  }

  def dequeue(): T = {
    val value = _delegate.peekFirst()
    value
  }

  def peek(): T = _delegate.peekFirst()
  def removeFirst(): Unit = _delegate.removeFirst()

  def elements: collection.Seq[T] = {
    val it = _delegate.iterator
    val r = new collection.mutable.ArrayBuffer[T](_delegate.size)
    var i = 0
    while (it.hasNext) { r += it.next }
    r
  }

  /** Apply op to each queued element */
  def forEach(op: T => Unit): Unit = {
    val it = _delegate.iterator
    while (it.hasNext) { op(it.next) }
  }

  def clear(): Unit = _delegate.clear()

}
