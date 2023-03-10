package io.threadcso.semaphore

/** Trait inherited by all semaphore implementations. All semaphores have
  * available a ''virtual'' finite collection of tokens that their clients may
  * acquire and release.
  */
trait Semaphore {

  /** Wait until a token is free, then remove it from the collection. */
  def acquire(): Unit

  /** Release a token that may (but need not) have previously been acquired. */
  def release(): Unit

  /** Optional: Wait until a token is free, or until `timeoutNS` nanoseconds
    * have elapsed. In the former case acquire a token and return true; in the
    * latter case return false.
    */
  def tryAcquire(timeoutNS: Long): Boolean =
    throw new UnsupportedOperationException()

  /** same as acquire */
  @inline def down(): Unit = acquire()

  /** same as release */
  @inline def up(): Unit = release()

  /** Return an (approximate) list of the threads waiting to acquire a token
    * from this semaphore. Intended for use only by diagnostic components.
    */
  def getWaiting: collection.Seq[Thread] =
    throw new UnsupportedOperationException()

  /** Approximation to the number of acquires that could now succeed without
    * having to wait. Intended for use only by diagnostic components.
    */
  def remaining: Int = 0

  /** Optional: cancel all threads waiting to acquire the semaphore: defined
    * only in [[io.threadcso.semaphore.BooleanSemaphore]] and
    * [[io.threadcso.semaphore.CountingSemaphore]]
    */
  def cancel(): Unit = throw new UnsupportedOperationException

  /** True if the most recent semaphore acquisition was cancelled */
  def cancelled(): Boolean = throw new UnsupportedOperationException()

}
