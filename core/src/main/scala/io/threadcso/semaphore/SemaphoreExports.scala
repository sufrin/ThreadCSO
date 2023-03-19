package io.threadcso.semaphore

trait SemaphoreExports {

  type Semaphore = io.threadcso.semaphore.Semaphore
  type Latch = io.threadcso.semaphore.Latch
  type Flag = io.threadcso.semaphore.Flag

  /** Factory for `BooleanSemaphore`s. @see
    * [[io.threadcso.semaphore.BooleanSemaphore]]
    */
  val BooleanSemaphore = io.threadcso.semaphore.BooleanSemaphore

  /** A semaphore suitable for mutual exclusion. */
  def MutexSemaphore(fair: Boolean = true) =
    BooleanSemaphore(available = true, fair = fair)

  /** A signalling semaphore. */
  def SignallingSemaphore(fair: Boolean = true) =
    BooleanSemaphore(available = false, fair = fair)

  /** Factory for `CountingSemaphore`s. @see
    * [[io.threadcso.semaphore.CountingSemaphore]]
    */
  val CountingSemaphore = io.threadcso.semaphore.CountingSemaphore

  /** Factory for `Latch`es */
  val Latch = io.threadcso.semaphore.Latch

  /** Factory for `Flag`s. */
  val Flag = io.threadcso.semaphore.Flag

}
