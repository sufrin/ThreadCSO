package io.threadcso.lock

import io.threadcso.semaphore.BooleanSemaphore

/**
A `Barrier(n)` supports the ''repeated'' synchronization of `n` processes. If `b` is
such a barrier then `b.sync` calls are stalled until `n` have been made.
When `n==1` then `b.sync` returns immediately: this is so multi-worker
structures can be tested with only a single-worker, and may be helpful
when testing cellular automata.

This implementation is simple-minded.
{{{
 \$Revision: 239 $
 \$Date: 2017-10-12 18:54:09 +0100 (Thu, 12 Oct 2017) $
}}}
*/

class Barrier(n: Int, name: String="")
{
  assert(n>=1)
  private [this] val shared  = n>1
  private [this] var waiting = 0 // number of processes currently waiting
  private [this] val wait    = BooleanSemaphore(available=false, name=s"$name.wait")
  private [this] val enter   = BooleanSemaphore(available=true,  name=s"$name.enter")
  // enter is up iff a new batch of processes can enter the barrier
  // each entering process but the last is stalled by wait

  /** Wait until all `n` processes have called sync */ 
  def sync(): Unit = if (shared)
  {
    enter.down()
    if (waiting==n-1)    // the last process arrives
      wait.up()          // everyone can proceed (but cannot re-enter)
    else                 // a process arrives that isn't the last
    { waiting+=1
      enter.up()
      wait.down()         // make it wait
      waiting-=1        
      if (waiting==0) 
         enter.up()       // the last waiting process awoke
      else 
         wait.up()        // pass the baton to another waiter
    }
  }
}
    

