package io.threadcso.process

/**
        A `PROC` is a prototype for a process that can be started and run.
        In ''Threaded CSO'' a `PROC` is started by acquiring a thread (usually
        from a thread pool). In ''Fibred CSO'' `PROC` is started by acquiring a
        fibre from the runtime system.
*/
trait PROC extends (() => Unit)
{ /** Run the process in the current thread */
  def apply(): Unit
  /** Acquire a thread, and start the process running in it. Return
      a handle on the running process.
  */
  def fork: Process.Handle

  override def toString: String = name
  protected [threadcso]
  var __name:      String = _
  protected [threadcso]
  var __stackSize: Long   = _
  /** Set the (advisory) stack size for threads in which
      this `PROC` will run to `stackSize`, and return `this`
      `PROC`. Some `JVM`s allow threads to be started with a
      given stack size.
  */
  def withStackSize(_stackSize: Long): PROC = { __stackSize = _stackSize; this }
  def stackSize: Long = __stackSize
  def name: String = __name
  /** Set the name of this `PROC` and return `this` `PROC`. */
  def withName(_name: String): PROC = { __name = _name; this }
  /** Syntactic parallel composition of `this` with `other`.*/
  def ||(other: PROC): PROC = new ParSyntax(List(other, this)) 
}
