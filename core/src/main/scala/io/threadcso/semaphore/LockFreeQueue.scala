package io.threadcso.semaphore

/** A (relatively) efficient lock-free queue that doesn't permit barging.
  * Currently implemented as a facade for
  * `jdk.util.concurrent.ConcurrentLinkedQueue`.
  *
  * @todo
  *   reimplement as a simpler variant of
  *   `java.util.concurrent.ConcurrentLinkedDeque`.
  */
sealed private[semaphore] class LockFreeQueue[T]
    extends java.util.concurrent.ConcurrentLinkedQueue[T]
    with Queue[T] { // private   [this] val _delegate: java.util.concurrent.ConcurrentLinkedQueue[T] = this

  @inline def length: Int = size

  @inline def enqueue(value: T): Unit = {
    add(value)
    ()
  }

  @inline def enqueueFirst(value: T): Unit = {
    add(value)
    ()
  }

  @inline def dequeue(): T = poll()

  @inline def removeFirst(): Unit = remove()

  def elements: collection.Seq[T] = {
    val it = iterator
    val r = new collection.mutable.ArrayBuffer[T](length)
    var i = 0
    while (it.hasNext) { r += it.next }
    r
  }

  /** Apply op to each queued element */
  def forEach(op: T => Unit): Unit = {
    val it = iterator
    while (it.hasNext) { op(it.next) }
  }

}
