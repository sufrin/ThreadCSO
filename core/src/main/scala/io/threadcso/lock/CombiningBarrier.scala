package io.threadcso.lock

/** A `CombiningBarrier(n, e, _ ⊕ _)` supports the (repeated) synchronization of
  * `n` processes. If `b` is such a barrier then `b.sync(x)` calls are stalled
  * until `n` have been made. We say that such a call ''contributes'' `x`. If
  * the syncing calls contribute `x1`, `x2`, ... `xn` then the value they all
  * return is `e ⊕ x1 ⊕ ... ⊕ xn`. The function `⊕` must be associative.
  */
class CombiningBarrier[T](n: Int, e: T, op: (T, T) => T, name: String = "")
    extends GenericLinearBarrier[T](n, e, op, name) {}
