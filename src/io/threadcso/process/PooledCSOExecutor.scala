package io.threadcso.process

import java.util.concurrent.Executors

/**
  * The simplest form of `CSOExecutor` just wraps a `java.util.concurrent.ExecutorService`.
*/
class PooledCSOExecutor(pool: java.util.concurrent.ExecutorService)
      extends CSOExecutor
{  /** Execute the `Runnable`. The `stackSize` is ignored. */
   def execute(runnable: java.lang.Runnable, stackSize: Long): Unit = { pool.execute(runnable) }
   def shutdown(): Unit = {}
}
