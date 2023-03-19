package io.threadcso.lock

trait GenericBarrier[T] {
  def sync(id: Int, value: T): T
}

/**   - GenericBarrier
  *     - LinearGenericBarrier
  *       - Barrier (LinearGenericBarrier[Unit])
  *       - barriers returning results
  *     - Log Generic Barrier
  *       - LogBarrier (LogGenericBarrier[Unit])
  *       - barriers returning results
  *
  * Quesstions:
  *   - should sync() be allowed in LinearGenericBarrier[Unit]?
  *   - should sync(t: T) be allowed in LogBarrier?
  *
  * Answers:
  *   - In terms of 
  */
