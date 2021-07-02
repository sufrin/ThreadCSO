package io.threadcso.channel
import io.threadcso.semaphore.{BooleanSemaphore, CountingSemaphore, Semaphore}
import java.util.concurrent.atomic.AtomicInteger

import io.threadcso.Nanoseconds

/**
  * A blocking queue used in the implementation of buffers. If `bound==0` then the queue is unbounded  and `put` will
  * never be blocked.
  *
  * @param bound    (if positive) bound on the size of the queue: a put will stall if the queue reaches this size.
  * @param parent_  the component whose implementation uses this queue (for the debugger)
  * @tparam T       the type of value in the queue
  *
  */


private [channel] class BlockingQueue[T](var bound: Int, parent_ : AnyRef, name: String)
{
  private [this] val homeGrown = true // Jan 9th 2018 ==> false works for MagicSquares

  private [this] val space: Semaphore =
    if (homeGrown)
             new CountingSemaphore(bound, s"$name.space", false, parent=parent_, spin=10)
       else
             new io.threadcso.semaphore.jdk.CountingSemaphore(bound, s"$name.space", false, true) //parent=parent_, spin=0)
  private [this] val data  =
     if (homeGrown)
        new CountingSemaphore(0, s"$name.data", false,  parent=parent_, spin=10)
     else
        new io.threadcso.semaphore.jdk.CountingSemaphore(0, s"$name.data", false, true) // parent=parent_, spin=0)

  /*
     Representation: a singly-linked list with dummy header
   */
  private class Node[TY](var value: TY, @volatile var next: Node[TY])
  private [this] var length        = 0
  private [this] var head: Node[T] = new Node[T](null.asInstanceOf[T], null)
  private [this] var last: Node[T] = head

  // short-lived locks on head and tail that will not deschedule except under much contention
  private [this] val enqMutex = BooleanSemaphore(true, null, fair=false, parent_, spin=3)
  private [this] val deqMutex = BooleanSemaphore(true, null, fair=false, parent_, spin=3)

  /** Clear the queue and release/cancel any threads waiting to put or to get */
  def clear(): Unit = {
    enqMutex.cancel()
    deqMutex.cancel()
    space.cancel()
    data.cancel()
     last=head; head.next=null
  }

  @inline def isEmpty: Boolean = head.eq(last)

  /** APPROXIMATE number of queued elements */
  @inline def size: Int = length // { enqMutex.acquire; try { length } finally { enqMutex.release } }

  /** Atomically add the given value to the end of the list */
  @inline private [this] def putInList(value: T): Unit =
  { val newlast = new Node(value, null)
    enqMutex.acquire()
    last.next = newlast
    length += 1
    last = newlast
    enqMutex.release()
  }

  /** Put the given `value` in the queue; stalling if necessary until space is available */
  @inline def put(value: T): Unit =
  { if (bound>0) space.acquire()
    if (space.cancelled()) throw new Closed(name)
    //
    putInList(value)
    //
    data.release()
    ()
  }

  /** If space is available before `nsWait` elapses, put `value` in the queue, and return true;
    * otherwise return false. This always returns true for an unbounded buffer.
    * */
  @inline def putBefore(nsWait: Nanoseconds, value: T): Boolean =
  {
    if (bound>0) {
      if (space.tryAcquire(nsWait)) {
         if (space.cancelled()) throw new Closed(name)
         putInList(value)
         true
      } else false
    } else {
        put(value)
        true
    }
  }

  /** If a datum (`v` say) is available at the front of the queue before `nsWait` elapses, then dequeue it and return
    * `Some(v)`; otherwise return `None`
    * */
  @inline def takeBefore(nsWait: Nanoseconds): Option[T] =
  {
    if (data.tryAcquire(nsWait))
      { if (data.cancelled()) throw new Closed(name)
        Some(takeFromList())
      }
    else
      None
  }

  /** Unconditionally and atomically remove and return the first element of the list */
  @inline private [this] def takeFromList(): T =
  {
    enqMutex.acquire()
    deqMutex.acquire()
    val first = head.next
    val result = first.value
    head.next  = first.next
    first.value = null.asInstanceOf[T] // for GC

    length -= 1
    if (first eq last)
    { last      = head
      head.next = null
    }

    deqMutex.release()
    enqMutex.release()
    result
  }


  /** Remove and return the longest-waiting value in the queue; stalling if necessary until one is available */
  @inline def take(): T =
  { data.acquire()
    if (data.cancelled()) throw new Closed(name)
    val result = takeFromList()
    if (bound > 0) space.release()
    result
  }

  /** Remaining capacity of the queue before `put` will stall. */
  @inline def remainingCapacity: Int = space.remaining
}

