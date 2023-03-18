package io.threadcso.lock

/** A `CombiningBarrier` with `e=true` and `op=_&&_` */

class AndBarrier(n: Int, name: String = "AndBarrier")
    extends LinearBarrier[Boolean](n, true, _ && _, name)
