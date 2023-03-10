package io.threadcso.process

/** A form of `CSOExecutor` that can retire unused pooled threads. If `report`
  * is true, then a brief report to be made to the console when the pool shuts
  * down (usually when the CSO program finishes). The report details the current
  * and the largest size of the pool, the number of processes executed.
  */
class ThreadPooledCSOExecutor(
    report: Boolean,
    pool: java.util.concurrent.ThreadPoolExecutor,
    stackSize: Long = 0
) extends CSOExecutor {
  @inline private def wasActive = pool.getTaskCount > 0

  def execute(runnable: java.lang.Runnable, stackSize: Long): Unit = {
    pool.execute(runnable)
  }

  def shutdown(): Unit = {
    pool.shutdown()
    if (report && wasActive) {
      if (stackSize == 0)
        Console.println("[Remaining pooled threads]")
      else
        Console.println("[Threads with stack size:  <=%d]".format(stackSize))
      Console.println(
        "[Total processes executed: %d]".format(pool.getTaskCount)
      )
      Console.println("[Current thread pool size: %d]".format(pool.getPoolSize))
      Console.println(
        "[Largest thread pool size: %d]".format(pool.getLargestPoolSize)
      )
    }
  }
}
