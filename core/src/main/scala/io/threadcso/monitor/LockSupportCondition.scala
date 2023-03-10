package io.threadcso.monitor

import java.util.concurrent.locks.LockSupport

class LockSupportCondition(lock: java.util.concurrent.locks.Lock) {

  /** Information about waiting threads. */
  private class ThreadInfo {
    val thread = Thread.currentThread // the waiting thread
    var ready = false // has this thread received a signal?
  }

  /** Queue holding ThreadInfos for waiting threads. This is accessed only by a
    * thread holding lock.
    */
  private val waiters = new scala.collection.mutable.Queue[ThreadInfo]()

  /** Wait on this condition. */
  def await(): Unit = {
    var wasInterrupted = false
    // record that I'm waiting
    val myInfo = new ThreadInfo; waiters.enqueue(myInfo)
    lock.unlock // release the lock
    while (!myInfo.ready) {
      LockSupport.park() // wait to be woken
      if (Thread.interrupted) { myInfo.ready = true; wasInterrupted = true }
    }
    lock.lock // reacquire the lock
    if (wasInterrupted)
      Thread.currentThread.interrupt // reassert interrupt status
  }

  def await(test: => Boolean): Unit = while (!test) await()

  /** Signal to the first waiting thread. */
  def signal() = {
    if (waiters.nonEmpty) {
      val threadInfo = waiters.dequeue()
      threadInfo.ready = true; LockSupport.unpark(threadInfo.thread)
    }
  }

  /** Signal to all waiting threads. */
  def signalAll() = {
    while (waiters.nonEmpty) {
      val threadInfo = waiters.dequeue()
      threadInfo.ready = true; LockSupport.unpark(threadInfo.thread)
    }
  }

}
