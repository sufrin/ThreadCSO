package io.threadcso.process

/**
        A form of `CSOExecutor` that can retire unused pooled threads, organized
        as a collection of pools of threads with similar stack-sizes. Apart from the default
        pool, pools are not activated until a process requesting a size of stack that falls within the range of
        that pool is started. The boundaries of the ranges are at: `256, 1024, 4096, 16384, 65536`.
        Processes specifying stack sizes outside those ranges (or leaving stacksize unspecified) are allocated from
        the default pool.
*/
class SizePooledCSOExecutor(makePool: Long => ThreadPooledCSOExecutor, report: Boolean)
      extends CSOExecutor
{
  val sizes  = Array( 1L<<8,  1L<<10,  1L<<12,  1L<<14,  1L<<16)
  val N: Int = sizes.length
  val pools: Array[ThreadPooledCSOExecutor] = sizes.map({ size => makePool(size) })
  val other: ThreadPooledCSOExecutor = makePool(0)

   def execute(runnable: java.lang.Runnable, stackSize: Long): Unit =
   { var i = 0
     if (stackSize==0) { other.execute(runnable, 0) } else
     {
       while (i<N && stackSize > sizes(i)) i+=1
       // i=N || stackSize <= sizes(i)
       if (i==N)
          other.execute(runnable, 0)
       else
          pools(i).execute(runnable, stackSize)
     }
   }

   def shutdown(): Unit =
   {
     for (pool<-pools.reverse) if (report) pool.shutdown()
     if (report) other.shutdown()
   }



}
