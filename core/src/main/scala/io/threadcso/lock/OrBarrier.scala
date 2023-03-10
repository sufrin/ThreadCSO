package io.threadcso.lock

/** A `CombiningBarrier` with `e=false` and `op=_||_` */

class OrBarrier(n: Int, name: String = "OrBarrier")
    extends CombiningBarrier[Boolean](n, false, _ || _, name)
