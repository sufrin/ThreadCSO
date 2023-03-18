package io.threadcso.lock

import java.util.concurrent.locks.ReentrantLock

/** <p> Reentrant lock implementation that uses the
  * `jdk.util.concurrent.lock.ReentrantLock` class. If `fair` is true then the
  * lock is first-come first-served. Under heavy contention this can be useful,
  * but it imposes a significant performance penalty on the acquisition of the
  * lock.
  */
class JavaLock(fair: Boolean) extends ReentrantLock(fair) with Lock {

  /** Try the lock for up to `timeoutNS`, and if it lock then evaluate `body`
    * else evaluate `otherwise`
    */
  @inline def tryLockFor[T](timeoutNS: Long)(body: => T)(otherwise: => T): T = {
    if (
      tryLock || tryLock(timeoutNS, java.util.concurrent.TimeUnit.NANOSECONDS)
    )
      try { body }
      finally { unlock() }
    else otherwise
  }

  /** Return an approximate list of the threads waiting to acquire this lock */
  def getWaiting: collection.Seq[Thread] = {
    // import scala.collection.convert.WrapAsScala.collectionAsScalaIterable
    // import scala.collection.JavaConverters._
    import scala.jdk.CollectionConverters._
    getQueuedThreads.asScala.toList
  }

  /** Approximate number of threads waiting to acquire this Lock */
  def numWaiting: Int = getQueueLength

}
