package io.threadcso.semaphore

/**
  * Queues used in the implementation of CSO Semaphores.
  *
  * @tparam T the type of value enqueued
  */

private [semaphore] trait Queue[T]
{ /** Place `value` at the end of the queue */
  def enqueue(value: T): Unit
  /** Place `value` in the queue: at the end if the queue doesn't permit barging */
  def enqueueFirst(value: T): Unit
  /** Remove and return the first element in the queue (or null if the queue is empty) */
  def dequeue():        T
  /** Return the first element in the queue (or null if the queue is empty) */
  def peek():           T
  /** Remove the first element in the queue if it's nonempty */
  def removeFirst():    Unit
  /** Approximation to the current length of the queue: O(n) */
  def length: Int
  /** Approximation to the current sequence of elements in the queue (for debugging) */
  def elements:         collection.Seq[T]
  /** Clear the   queue */
  def clear(): Unit
  /** Apply `op` to each element in the queue */
  def forEach(op: T=> Unit): Unit
}
