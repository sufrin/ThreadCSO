package io.threadcso.lock

import io.threadcso.semaphore.BooleanSemaphore
//import io.threadcso.semaphore.jdk.BooleanSemaphore

/** A `CombiningBarrier(n, e, _ ⊕ _)` supports the (repeated) synchronization of
  * `n` processes. If `b` is such a barrier then `b.sync(x)` calls are stalled
  * until `n` have been made. We say that such a call ''contributes'' `x`. If
  * the syncing calls contribute `x1`, `x2`, ... `xn` then the value they all
  * return is `e ⊕ x1 ⊕ ... ⊕ xn`. The function `⊕` must be associative.
  *
  * {{{
  * $Revision: 239 $
  * $Date: 2017-10-12 18:54:09 +0100 (Thu, 12 Oct 2017) $
  * }}}
  */

class CombiningBarrier[T](n: Int, e: T, op: (T, T) => T, name: String = "")
    extends Barrier[T] {
  assert(n > 1)

  private[this] var waiting = 0 // number of processes currently waiting
  
  private[this] val wait =
    BooleanSemaphore(available = false, name = s"$name.wait")

  private[this] val enter =
    BooleanSemaphore(available = true, name = s"$name.enter")

  // enter is up iff a new batch of processes can enter the barrier
  // each entering process but the last is stalled by wait
  private[this] var result = e

  /** Wait until all `n` processes have called sync, then return the value
    * calculated with `op`, and `e` from the individual processes'
    * contributions.
    */
  def sync(id: Int, t: T): T = {
    enter.down()
    result = op(result, t) // add the last contribution
    if (waiting == n - 1) // the last process arrives
      try { result }
      finally { wait.up() } // everyone can proceed (but cannot re-enter)
    else // a process arrives that isn't the last
      {
        waiting += 1
        enter.up() // another can enter
        wait.down() // the caller waits
        waiting -= 1 // after the wait finishes
        try { result } // return a result
        finally {
          if (waiting == 0) { result = e; enter.up() }
          else wait.up()
        }
      }
  }
}
