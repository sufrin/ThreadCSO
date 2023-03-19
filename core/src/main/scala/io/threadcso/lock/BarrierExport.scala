package io.threadcso.lock

trait BarrierExports {
  type Barrier = io.threadcso.lock.LinearBarrier
  type LogBarrier = io.threadcso.lock.LogBarrier
  type CombiningBarrier[T] = io.threadcso.lock.CombiningBarrier[T]

  /** Factory for `Barrier`s */
  def Barrier(n: Int, name: String = ""): Barrier =
    new LinearBarrier(n, name)

  def LogBarrier(n: Int, name: String = ""): LogBarrier =
    new LogBarrier(n, name)

  /** Factory for `CombiningBarrier`s */
  def CombiningBarrier[T](
      n: Int,
      e: T,
      f: (T, T) => T,
      name: String = ""
  ): CombiningBarrier[T] =
    new CombiningBarrier[T](n, e, f, name)
}
