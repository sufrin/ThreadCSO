package io.threadcso.process

/** A `PROC` is a prototype for a process that can be started and run. In
  * ''Threaded CSO'' a `PROC` is started by acquiring a thread from
  * an executor: this may be orthodox thread pool or (in later implementations)
  * source of virtual threads.
  *
  * The executor may be specified by `withExecutor` as the `PROC` is constructed,
  * or (as is much more usual) may be provided by default just as the `PROC`
  * starts running.
  */
trait PROC extends (() => Unit) {

  /** Run the process in the current thread */
  def apply(): Unit

  /** Acquire a thread, and start the process running in it. Return a handle on
    * the running process.
    */
  def fork: Process.Handle

  override def toString: String = name
  protected[threadcso] var __name: String = _
  protected[threadcso] var __stackSize: Long = _

  /** Set the (advisory) stack size for threads in which this `PROC` will run to
    * `stackSize`, and return `this` `PROC`. Some `JVM`s allow threads to be
    * started with a given stack size.
    */
  def withStackSize(_stackSize: Long): PROC = { __stackSize = _stackSize; this }
  def stackSize: Long = __stackSize
  def name: String = __name
  /** Set the name of this `PROC` and return `this` `PROC`. */
  def withName(_name: String): PROC = { __name = _name; this }

  
  protected[threadcso] var executor: CSOExecutor = null
  /**
   * Set the executor (see CSOThreads for possible values).
   */
  def withExecutor(anExecutor: CSOExecutor): PROC = {
      executor = anExecutor
      this
  }

  /**
   * Set the executor (see CSOThreads for possible values).
   */
  def withExecutor(poolKind: String): PROC =
      withExecutor(io.threadcso.process.CSOThreads.getExecutor(poolKind))


  /** Syntactic parallel composition of `this` with `other`. */
  def ||(other: PROC): PROC = new ParSyntax(List(other, this))
}
