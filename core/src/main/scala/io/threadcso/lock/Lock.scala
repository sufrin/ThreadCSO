package io.threadcso.lock

trait Lock {

  /** evaluate body with the lock set; finally unset the lock */
  @inline def withLock[T](body: => T): T = try { lock(); body }
  finally { unlock() }
  def lock(): Unit
  def unlock(): Unit
}
