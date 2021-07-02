package io.threadcso.process

/**
       A `CSOExecutor` is the means by which (possibly-pooled)
       threads are allocated to the `Runnable`s  that correspond
       to `CSO` processes in order to run them. It is an abstraction
       of some sort of execution service.
*/

trait CSOExecutor
{
   /** Acquire a thread and run the given `Runnable` in it, expressng a preference
     * for a stack of the specified `stackSize` (or the default size if `stackSize==0`) */
   def execute(runnable: java.lang.Runnable, stackSize: Long): Unit

   /** Take any measures needed to close down the service */
   def shutdown(): Unit
}
