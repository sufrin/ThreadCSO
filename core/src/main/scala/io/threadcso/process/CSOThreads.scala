package io.threadcso.process

/** This is the module in which the characteristics of the pooling of threads
  * used by threaded-`CSO` are determined.
  *
  * To be specific, the following jdk runtime properties determine
  * thread-pooling characteristics, and can be set either from a program that
  * uses CSO (before any CSO process construct is executed) or (as we usually
  * expect) by setting the appropriate jdk property from the (usually scala)
  * command line that runs the program.
  *
  *   - `io.threadcso.pool.SECS` the time before pooled threads are deemed idle
  *     (ADAPTIVE and SIZED pools only). See [[CSOThreads.poolSECS]]
  *   - `io.threadcso.pool.KIND` the kind of the pool: ADAPTIVE, SIZED, CACHED,
  *     UNPOOLED. See [[CSOThreads.poolKIND]]
  *   - `io.threadcso.pool.REPORT` report pool performance on program exit. See
  *     [[CSOThreads.poolREPORT]]
  *
  * The following runtime property has a specialized interpretation.
  *
  *   - `io.threadcso.pool.stack` specifies the default stacksize for threads
  *     made from ADAPTIVE and CACHED pools. See [[CSOThreads.poolSTACKSIZE]]
  *
  * It is specified by `io.threadcso.pool.stack=`''n'' where ''n'' is a
  * (possibly real) number, possibly followed by `G`, `K`, or `M` in upper or
  * lower case (default is `K`).
  *
  * More details about the virtual threads can be found here:
  *   - [https://openjdk.org/jeps/425]
  *   - [https://download.java.net/java/early_access/jdk20/docs/api/java.base/java/lang/Thread.html#start()]
  */
object CSOThreads {

  import java.util.concurrent._
  import io.threadcso.basis._

  private val mainThread = Thread.currentThread

  /** The thread group containing threads used to run CSO processes. */
  private val csoThreads                    = new ThreadGroup("CSO")

  /**
   *   The mapping of (live) vThread indexes to vThreads 
   */
  private var vThreads =
          new scala.collection.concurrent.TrieMap[Long, Thread]

  /**
    * PRE: size of CSO platform thread pools (or count of CSO threads) is not increasing
    * EFFECT: applies f to each active thread in the CSO pool.
    */
  def forActiveKThreads(f: Thread => Unit): Unit = {
    val count = 30 + csoThreads.activeCount // overestimate
    val threads = Array.ofDim[Thread](count)
    val actual = csoThreads.enumerate(threads)
    threads(actual) = mainThread
    for (i <- 0 until actual + 1) f(threads(i))
  }

  /**
    * PRE: size of CSO virtual thread pool is not increasing
    * EFFECT: applies f to each active virtual thread.
    */
  def forActiveVThreads(f: Thread => Unit) = {
    val it = vThreads.valuesIterator
    while (it.hasNext) f(it.next())
  }
  

  /** Make a thread factory for CSO threads of the specified size */
  private def factory(stackSize: Long) = new ThreadFactory {
    private val threadCount = new java.util.concurrent.atomic.AtomicLong

    def newThread(r: Runnable): Thread =
      new Thread(
        csoThreads,
        r,
        "cso-pool-%d[%d]".format(threadCount.getAndIncrement, stackSize),
        stackSize
      )
  }

  /** Construct a single ADAPTIVE pooled executor to generate threads with
    * stacks with the recommended `stackSize`.
    */
  private def sizePooledCSOExecutor(stackSize: Long) = {
    import java.util.concurrent._
    val pool =
      new ThreadPoolExecutor(
        poolMIN,
        poolMAX,
        poolSecs,
        TimeUnit.SECONDS,
        new SynchronousQueue[Runnable](),
        factory(stackSize)
      )
    new ThreadPooledCSOExecutor(poolREPORT, pool, stackSize)
  }

  /** Setting a jdk property with `-Dio.threadcso.pool=`''secs'' specifies that
    * idling pooled threads expire after ''secs'' idling. (deprecated: default
    * 4)
    */
  val poolSecs: Long =
    getPropElse("io.threadcso.pool", _.toLong)(4L) // deprecated

  /** Setting a jdk property with `-Dio.threadcso.pool.SECS=`''secs'' specifies
    * that idling pooled threads expire after ''secs'' idling. Default same as
    * `poolSecs`.
    */
  val poolSECS: Long = getPropElse("io.threadcso.pool.SECS", _.toLong)(poolSecs)

  /** Setting a jdk property with `-Dio.threadcso.pool.MAX=`''number'' specifies
    * an upper bound on the number of threads to be kept in ADAPTIVE or SIZED
    * pools. Leave it at its default value unless you have understood the
    * documentation of `ThreadPoolExecutor`.
    */
  val poolMAX: Int =
    getPropElse("io.threadcso.pool.MAX", _.toInt)(Integer.MAX_VALUE)

  /** Setting a jdk property with `-Dio.threadcso.pool.MIN=`''number'' specifies
    * a lower bound on the number of threads to be kept in ADAPTIVE or SIZED
    * pools. Leave it at its default value unless you have understood the
    * documentation of `ThreadPoolExecutor`.
    */
  val poolMIN: Int = getPropElse("io.threadcso.pool.MIN", _.toInt)(0)


  /** Setting a jdk property with `-Dio.threadcso.pool.REPORT=true` causes
    * adaptive pools to report on their activity when `ox.CSO.exit` is called,
    * and/or the pool(s) are shut down.
    */
  val poolREPORT: Boolean =
    getPropElse("io.threadcso.pool.REPORT", _.toBoolean)(false)

  /** Life's too short to construct lovely prose. This is how to recommend the
    * stack sizes of threads made with CACHED or ADAPTIVE pooling.
    * `-Dio.threadcso.pool.stack=`''n''`k` or ''n''m` or ''n''`g`, where ''n''
    * is a (possibly real) number.
    */
  val poolSTACKSIZE: Long = {
    def getSize(s: String): Long = {
      var r = 0.0d
      val (n, m) = s.last match {
        case 'k' | 'K' => (s.take(s.length - 1).toDouble, 1000)
        case 'm' | 'M' => (s.take(s.length - 1).toDouble, 1000000)
        case 'g' | 'G' => (s.take(s.length - 1).toDouble, 1000000000)
        case _         => (s.toDouble, 1000)
      }
      (m.toDouble * n).ceil.toInt
    }

    try
      getPropElse("io.threadcso.pool.stack", getSize(_))(0)
    catch {
      case e: java.lang.Exception =>
        System.err.println(
          s"io.thread.pool.stack should be specified by m.nK, m.nG, m.nM, or m.n. $e"
        )
        throw e
    }
  }

   /**
    *   One of each standard kind of CSOExecutor
    */


   lazy val SIZED: CSOExecutor =
            new SizePooledCSOExecutor(sizePooledCSOExecutor, poolREPORT)

   lazy val ADAPTIVE: CSOExecutor =
            sizePooledCSOExecutor(poolSTACKSIZE)

   lazy val CACHED: CSOExecutor =
        new PooledCSOExecutor(Executors.newCachedThreadPool(factory(poolSTACKSIZE)))
        
   lazy val UNPOOLED: CSOExecutor =
        new CSOExecutor {
            private val threadCount = new java.util.concurrent.atomic.AtomicLong
            def execute(r: Runnable, stackSize: Long): Unit =
              new Thread(
                csoThreads,
                r,
                "cso-unpooled-%d".format(threadCount.getAndIncrement),
                stackSize
              ).start()

            def shutdown(): Unit = {}
        }

   /**
    *  This executor keeps track of the currently-running virtual threads, at a slight cost
    *  in their startup and closedown times. 
    */
   lazy val VIRTUAL:  CSOExecutor = new CSOExecutor {
            private val threadCount   = new java.util.concurrent.atomic.AtomicLong
            private val threadBuilder = Thread
                                          .ofVirtual()
                                          .name("cso-virtual-%d".format(threadCount.getAndIncrement))
            def execute(r: Runnable, stackSize: Long): Unit = {
              val proxy = new Runnable {
                def run(): Unit = {
                  val current = Thread.currentThread
                  val id      = current.threadId
                  vThreads    += ((id, current))
                  try {
                    r.run()
                  } catch {
                    case thr: Throwable => vThreads.remove(id); throw thr
                  }
                }
              }
              threadBuilder.start(proxy)
            }

            def shutdown(): Unit = {}
          }
   /**
    *  This executor doesn't keep track of the currently-running virtual threads. They start a mite faster
    *  than VIRTUAL threads but the debugger doesn't get to see them
    */
   lazy val FASTVIRTUAL:  CSOExecutor = new CSOExecutor {
            private val threadCount   = new java.util.concurrent.atomic.AtomicLong
            private val threadBuilder = Thread
                                          .ofVirtual()
                                          .name("cso-virtual-%d".format(threadCount.getAndIncrement))
            def execute(r: Runnable, stackSize: Long): Unit = threadBuilder.start(r)

            def shutdown(): Unit = {}
          }
  
  /**
    * Setting a jdk property with `-Dio.threadcso.pool.KIND=`''kind'' determines what kind
    * of thread pooling to use by default:
    *   - `ADAPTIVE` -- Retires idle threads after `poolSECS`.
    *     See [[SizePooledCSOExecutor]].
    *   - `SIZED` -- runs several ADAPTIVE pools: each with threads of a similar
    *     stack size. See [[SizePooledCSOExecutor]].
    *   - `CACHED` -- a pool that never retires idle threads.
    *   - `UNPOOLED` -- makes a new thread to start and run a `PROC`. Stacksize
    *     is as specified by the `PROC`.
    *   - `VIRTUAL` -- makes a new virtual thread to start and run a `PROC`. Stacksize
    *     is as specified by the `PROC`. 
    *   - `FASTVIRTUAL` -- makes a new virtual thread to start and run a `PROC`. Stacksize
    *     is as specified by the `PROC`. The thread is not visible to the debugger.
    *
    *   Default is VIRTUAL
    */
    
  def poolKIND: String =
    getPropElse("io.threadcso.pool.KIND", { s => s })("VIRTUAL")

  /**  The current default `CSOExecutor` used in this program.
    *  Its kind is determined by `poolKIND`.
    */
   def executor(): CSOExecutor = getExecutor(poolKIND)

   def getExecutor(poolKind: String): CSOExecutor =
   poolKind.toUpperCase match {
      case "SIZED"        => SIZED
      case "ADAPTIVE"     => ADAPTIVE
      case "CACHED"       => CACHED
      case "UNPOOLED"     => UNPOOLED
      case "VIRTUAL"      => VIRTUAL
      case "FASTVIRTUAL"  => FASTVIRTUAL
   }

    /**
     *  Shut all executors down. 
     *  TODO: This could be somewhat more efficient. (Only shut down the used executors)
     *        It's only used to debrief the pooled executors.
     */
    def shutDown(): Unit =
    {
       for { ex <- List(SIZED,ADAPTIVE,CACHED,UNPOOLED,VIRTUAL) } ex.shutdown()
    }

}
