package io.threadcso.debug

trait DebugExports {

  type Debuggable = io.threadcso.debug.REGISTRY.Debuggable

  /** The debugger (if it is loaded), else null. The debugger's port is
    * specified by the integer ''N'' set as a runtime option by
    * `-Dio.threadcso.debugger.port=`''N'' (the default setting is zero). If
    * ''N'' is negative, the debugger is not loaded; if ''N'' is zero, the
    * debugger is loaded and made available at some currently-free port on the
    * local host; if ''N'' is positive then the debugger is loaded and made
    * available at port ''N'' if that port is free.
    */
  lazy val debugger: io.threadcso.debug.DEBUGGER = {
    val debuggerPort =
      io.threadcso.basis.getPropElse("io.threadcso.debugger.port", _.toInt)(0)
    if (debuggerPort < 0)
      null.asInstanceOf[io.threadcso.debug.DEBUGGER]
    else
      new io.threadcso.debug.DEBUGGER(debuggerPort)
  }
  {}

  /** A concrete debuggable class whose `toString` method evaluates the
    * expression `theFormat` whenever it is called (normally by the debugger).
    * This is a notationally convenient way of constructing a
    * debugger-registerable object that can be used to show (part of) the state
    * of a running process.
    *
    * For example, the copy process below keeps track of the number of values it
    * has copied, and of the last value. It registers a `DebuggerFormat` with
    * the debugger, so that if the debugger is invoked during the `repeat`, then
    * the current state of the process will be shown when the debugger is
    * showing its registered objects.
    *
    * {{{
    * def mycopy[T](in: ?[T], out: ![T]) = proc {
    *   var buf = in.nothing
    *   var n = 0
    *   val format = new DebuggerFormat(s"\$n: \$buf")
    *   format.withDebugger(true) {
    *     repeat { buf = in ? (); out ! buf; n += 1 }
    *   }
    * }
    * }}}
    *
    * The method `withDebugger` (see below) is a convenient way of constructing
    * and registering a debugger format to be used during the evaluation of an
    * expression.
    */
  class DebuggerFormat(theFormat: => String) extends Debuggable {
    override def toString: String = theFormat
  }

  /** Constructs a `DebuggerFormat` object from the given format string
    * expression and registers it (if `register` is true) with the debugger
    * during the evaluation of the `body` expression.
    *
    * For example, the copy process below keeps track of the number of values it
    * has copied, and of the last value. If the debugger is invoked during the
    * `repeat`, then the current state of the process will be shown when the
    * debugger is showing its registered objects.
    *
    * {{{
    * def mycopy[T](in: ?[T], out: ![T]) = proc {
    *   var buf = in.nothing
    *   var n = 0
    *   withDebuggerFormat(s"\$n: \$buf") {
    *     repeat { buf = in ? (); out ! buf; n += 1 }
    *   }
    * }
    * }}}
    */
  def withDebuggerFormat[T](theFormat: => String, register: Boolean = true)(
      body: => T
  ): T = {
    val format = new DebuggerFormat(theFormat)
    format.withDebugger(register) { body }
  }
}
