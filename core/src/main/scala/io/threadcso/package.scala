package io

import io.threadcso.alternation.Run

/** The standard `threadCSO` API. Most modules using `CSO` will need only the
  * declaration:
  * {{{
  * import io.threadcso._
  * }}}
  *
  * March 2023: changes 1.2 => 1.3
  *
  *   - virtual threads incorporated (requires jdk20)
  *
  * August 2017: changes 1.1 => 1.2
  *
  *   - renaming of very many internal classes and packages
  *
  *   - basic channel implementations are more efficient, in some case much more
  *     so
  *
  *   - alternation reliability improved
  *
  *   - debugger registration of alternations is no longer needed
  *
  *   - home-grown semaphores can specify which component they are part of: this
  *     makes interpreting a stack backtrace very much easier
  *
  *   - there is a flexible logging system that is compatible with the debugger
  *
  * April 2016: changes 1.0 => 1.1
  *
  *   - Extended rendezvous read operator is now ??` (was `?`)
  *
  *   - Extended rendezvous read event notation is now `=??=>` (was `=?=>>`)
  *
  *   - The notation ``inport ? f`` is now equivalent to `f(inport?())` This
  *     makes for a tidier layout when the function `f` is an explicit
  *     functional expression.
  *
  * Feb 1 2017: changes 1.1R1 => 1.1R2
  *
  *   - Removed dependencies on deprecated Java->Scala functions: replaced with
  *     .asJava
  *     {{{
  *         @author Bernard Sufrin, Oxford
  *         \$Revision: 286 $
  *         \$Date: 2017-11-18 17:41:30 +0000 (Sat, 18 Nov 2017) $
  *     }}}
  */
package object threadcso {
  import io.SourceLocation.SourceLocation
  import io.threadcso.alternation.event._
  import io.threadcso.process.{PROC => _, _} // we are exporting PROC

  ////////////////////////////// MACRO EXPORT
  import scala.language.experimental.macros

  /** Implicit parameter to various methods */
  implicit def sourceLocation: SourceLocation =
    macro io.SourceLocation.sourceLocationMACRO
  //////////////////////////////

  /** Provides the source location to pass as `(implicit loc: SourceLocation)`
    * parameters
    */
  // implicit def sourceLocation: SourceLocation = io.SourceLocation.sourceLocation

  type Nanoseconds = io.threadcso.basis.Nanoseconds
  type Milliseconds = io.threadcso.basis.Milliseconds

  ///////////////////////

  type Monitor = io.threadcso.monitor.Monitor
  type FairMonitor = io.threadcso.monitor.FairMonitor
  type Condition = io.threadcso.monitor.LockSupportCondition
  // type Condition   = java.util.concurrent.locks.Condition
  type Debuggable = io.threadcso.debug.REGISTRY.Debuggable

  /** A new `AbstractMonitor` object (either a `FairMonitor` or a `Monitor`)
    * whose lock-acquisition policy is as specified by `fair`.
    */
  def Monitor(
      fair: Boolean,
      name: String = null
  ): io.threadcso.monitor.AbstractMonitor =
    if (fair)
      new io.threadcso.monitor.FairMonitor(name)
    else
      new io.threadcso.monitor.Monitor(name)

  ///////////////////////

  type Semaphore = io.threadcso.semaphore.Semaphore
  type Latch = io.threadcso.semaphore.Latch
  type Flag = io.threadcso.semaphore.Flag
  type Barrier = lock.Barrier
  type CombiningBarrier[T] = lock.CombiningBarrier[T]
  type LogBarrier = lock.LogBarrier
  type CombiningLogBarrier[T] = lock.CombiningLogBarrier[T]

  /** Factory for `BooleanSemaphore`s. @see
    * [[io.threadcso.semaphore.BooleanSemaphore]]
    */
  val BooleanSemaphore = io.threadcso.semaphore.BooleanSemaphore

  /** A semaphore suitable for mutual exclusion. */
  def MutexSemaphore(fair: Boolean = true) =
    BooleanSemaphore(available = true, fair = fair)

  /** A signalling semaphore. */
  def SignallingSemaphore(fair: Boolean = true) =
    BooleanSemaphore(available = false, fair = fair)

  /** Factory for `CountingSemaphore`s. @see
    * [[io.threadcso.semaphore.CountingSemaphore]]
    */
  val CountingSemaphore = io.threadcso.semaphore.CountingSemaphore

  /** Factory for `Latch`es */
  val Latch = io.threadcso.semaphore.Latch

  /** Factory for `Flag`s. */
  val Flag = io.threadcso.semaphore.Flag

  /** Factory for `Barrier`s */
  def Barrier(n: Int, name: String = null): Barrier = new lock.Barrier(n, name)

  /** Factory for `CombiningBarrier`s */
  def CombiningBarrier[T](
      n: Int,
      e: T,
      f: (T, T) => T,
      name: String = null
  ): CombiningBarrier[T] =
    new CombiningBarrier[T](n, e, f, name)

  /** Factory for `LogBarrier`s */
  def LogBarrier(n: Int, name: String=null): LogBarrier = new LogBarrier(n, name)

  /** Factory for `CombiningLogBarrier`s */
  def CombiningLogBarrier[T](
                           n: Int,
                           e: T,
                           f: (T, T) => T,
                           name: String = null
                         ): CombiningLogBarrier[T] =
    new lock.CombiningLogBarrier[T](n, e, f, name)

  ///////////////////////

  /** Type of an '''alt-capable''' channel as returned by the standard factory
    * methods of the `io.threadcso` API.
    */
  type Chan[T] = io.threadcso.alternation.channel.Chan[T]

  /** A process that does nothing. */
  val SKIP = Process.SKIP

  /** Terminate the current process or the current `repeat` */
  @inline def stop = throw Process.stopped

  /** Process type */
  type PROC = io.threadcso.process.PROC

  /** Run the given `proc` in the current thread. */
  @inline def run(proc: PROC): Unit = proc()

  /** Run the given `proc` in an acquired thread, and return a handle on the
    * running thread.
    */
  @inline def fork(proc: PROC): Process.Handle = proc.fork

  /** {{{
    * proc ("name") { body }
    * }}}
    * is a `PROC`-valued expression denoting a simple process with the given
    * name.
    */
  @inline def proc(name: String)(body: => Unit): PROC =
    new Process.Simple(() => body).withName(name)

  /** {{{
    * proc { body }
    * }}}
    * is a `PROC`-valued expression denoting a simple process with a made-up
    * name.
    *
    * It is, in-effect, a procedure of type `()=>UNIT`.
    * {{{
    * run(proc { body }) = (proc { body })() = (()=>body)()
    * }}}
    */
  @inline def proc(body: => Unit): PROC =
    new Process.Simple(() => body).withName(Process.genName)

  /** Same as `proc(body)` */
  @inline def π(body: => Unit): PROC =
    new Process.Simple(() => body).withName(Process.genName)

  /** Same as `proc(name)(body)` */
  @inline def π(name: String)(body: => Unit): PROC =
    new Process.Simple(() => body).withName(name)

  /** Run the parallel composition of `procs` */
  @inline def par(procs: collection.Seq[PROC]): Unit =
    if (procs.isEmpty) {} else
      new Process.Par(s"""par(${procs mkString ","})""")(procs)()

  /** Same as `par(range.map(gen))` */
  @inline def par[T](range: collection.Seq[T])(gen: T => PROC): Unit =
    par(range.map(gen))

  /** The parallel composition of `procs`; unless `procs` is empty, in which
    * case `SKIP`.
    */
  @inline def ||(procs: collection.Seq[PROC]): PROC =
    if (procs.isEmpty) SKIP
    else new Process.Par(s"""||(${procs mkString ","})""")(procs)

  /** Same as `||(range.map(gen))` */
  @inline def ||[T](range: collection.Seq[T])(gen: T => PROC): PROC =
    ||(range.map(gen))

  /** Close down the thread pools, and exit the program. */
  @inline def exit(): Unit = Process.exit()

  /** Close down the thread pools, and exit the program, yielding `code`. */
  @inline def exit(code: Int): Unit = Process.exit(code)

  /** Number of nanoseconds in a nanosecond */
  val nanoSec: Nanoseconds = 1L

  /** Number of nanoseconds in a microsecond: `n*microSec` is n microseconds
    * expressed as nanoseconds
    */
  val microSec: Nanoseconds = 1000L * nanoSec

  /** Number of nanoseconds in a microsecond: `n*μS` is n microseconds expressed
    * as nanoseconds
    */
  val μS: Nanoseconds = microSec

  /** Number of nanoseconds in a millisecond: `n*milliSec` is n milliseconds
    * expressed as nanoseconds
    */
  val milliSec: Nanoseconds = 1000L * microSec

  /** Number of nanoseconds in a second: `n*Sec` is n seconds expressed as
    * nanoseconds
    */
  val Sec: Nanoseconds = 1000L * milliSec

  /** Number of nanoseconds in a minute: `n*Min` is n minutes expressed as
    * nanoseconds
    */
  val Min: Nanoseconds = 60L * Sec

  /** Number of nanoseconds in an hour: `n*Hour` is n hours expressed as
    * nanoseconds
    */
  val Hour: Nanoseconds = 60L * Min

  /** Number of nanoseconds in a day: `n*Day` is n days expressed as nanoseconds
    */
  val Day: Nanoseconds = 24L * Hour

  /** Convert a fractional time expressed in seconds to nanoseconds */
  def seconds(secs: Double): Nanoseconds =
    (secs * Sec).toLong

  /** Sleep for the given number of milliseconds. */
  @inline def sleepms(ms: Milliseconds): Unit = Thread.sleep(ms)

  /** Sleep for the given number of nanoseconds */
  @inline def sleep(ns: Nanoseconds): Unit =
    Thread.sleep(ns / milliSec, (ns % milliSec).toInt)

  /** Read the system nanosecond timer */
  @inline def nanoTime: Nanoseconds = System.nanoTime()

  /** Read the system millisecond timer */
  @inline def milliTime: Milliseconds = System.currentTimeMillis()

  /** Execute an alternation */
  def alt(events: Event)(implicit loc: SourceLocation): Unit =
    Alternation(false, events)(loc).alt()

  /** Execute an alternation */
  def prialt(events: Event)(implicit loc: SourceLocation): Unit =
    Alternation(false, events)(loc).alt()

  /** repeatedly execute an alternation -- ready events are chosen round-robin
    * -- approximating fairness
    */
  def serve(events: Event)(implicit loc: SourceLocation): Unit =
    Alternation(false, events)(loc).serve()

  /** repeatedly execute an alternation */
  def priserve(events: Event)(implicit loc: SourceLocation): Unit =
    Alternation(false, events)(loc).priserve()

  /** Compile an alternation. This can be useful for making a strength-reduction
    * optimization by taking the compilation/normalization of the body of the
    * alternation outside a loop. `val a = Alternation(events); ... ; a.alt`
    * (where `a` is a variable not occuring in ...) is always equivalent to `...
    * alt(events)`. Likewise for `prialt`, `priserve`, and `serve`.
    */
  @inline def Alternation(debug: Boolean, events: Event)(implicit
      loc: SourceLocation
  ) = new Run(debug, events, loc)

  /** Compile an alternation. This can be useful for making a strength-reduction
    * optimization by taking the compilation/normalization of the body of the
    * alternation outside a loop. `val a = Alternation(events); ... ; a.alt`
    * (where `a` is a variable not occuring in ...) is always equivalent to `...
    * alt(events)`. Likewise for `prialt`, `priserve`, and `serve`.
    */
  @inline def Alternation(events: Event)(implicit loc: SourceLocation) =
    new Run(false, events, loc)

  /** Execute an alt (while registered with the debugger, if `debug`) */
  def alt(debug: Boolean, events: Event)(implicit loc: SourceLocation): Unit =
    Alternation(debug, events)(loc).alt()

  /** Execute a prialt (while registered with the debugger, if `debug`) */
  def prialt(debug: Boolean, events: Event)(implicit
      loc: SourceLocation
  ): Unit = Alternation(debug, events)(loc).alt()

  /** repeatedly execute an alt (while registered with the debugger, if `debug`)
    * -- event choice is fair
    */
  def serve(debug: Boolean, events: Event)(implicit loc: SourceLocation): Unit =
    Alternation(debug, events)(loc).serve()

  /** repeatedly execute an alt (while registered with the debugger, if `debug`)
    */
  def priserve(debug: Boolean, events: Event)(implicit
      loc: SourceLocation
  ): Unit = Alternation(debug, events)(loc).priserve()

  /** after syntax for 'alt' bodies */
  val after: (=> Nanoseconds) => AfterDeadline =
    io.threadcso.alternation.event.after _

  /** orelse syntax for 'alt' bodies */
  val orelse: io.threadcso.alternation.event.orelse.type =
    io.threadcso.alternation.event.orelse

  /** prefix | syntax for 'alt' bodies */
  val | : (collection.Seq[ExecutableEvent]) => ExecutableEventSyntax =
    io.threadcso.alternation.event.|

  /////////////////////////////////////

  /** Evaluate `body` and return its value unless an exception ''ex'' is thrown.
    * If ''ex'' is a `Stopped` then evaluate and return the value of
    * `alternative`, otherwise re-throw ''ex''.
    */
  @inline def attempt[T](body: => T)(alternative: => T): T = {
    try { body }
    catch {
      case _: Stopped   => alternative
      case t: Throwable => throw t
    }
  }

  /** Iterate `body` while the evaluation of `guard` yields `true`. If an
    * exception ''ex'' is thrown, then stop the iteration, then unless ''ex'' is
    * a `Stopped` re-throw ''ex''.
    */
  @inline def repeat(guard: => Boolean)(body: => Unit): Unit = {
    var go = guard
    while (go)
      try { body; go = guard }
      catch {
        case _: Stopped   => go = false
        case t: Throwable => throw t
      }
  }

  /** Iterate `body`. If an exception ''ex'' is thrown, then stop the iteration,
    * and unless ''ex'' is a `Stopped` re-throw ''ex''.
    */
  @inline def repeat(body: => Unit): Unit = {
    var go = true
    while (go)
      try { body }
      catch {
        case _: Stopped   => go = false
        case t: Throwable => throw t
      }
  }

  /** `repeatFor (it: Iterable[T]) { bv => body }` applies the function `{ bv =>
    * body }` to each of the elements of an iterator formed from the iterable.
    * If an exception ''ex'' is thrown, then stop the iteration, then unless
    * ''ex'' is a `Stopped` re-throw ''ex''.
    */

  @inline def repeatFor[T](it: Iterable[T])(fn: T => Unit): Unit =
    attempt { for (t <- it) fn(t) } {}

  /** Returns true iff the current thread has been cancelled, and clears the
    * cancelled bit in the thread if so.
    */
  def interrupted(): Boolean = java.lang.Thread.interrupted()

  //////////////////////// Channel factory methods /////////////////////////

  import io.threadcso.alternation.channel.SharedAltCapableChannel
  type ??[T] = io.threadcso.alternation.channel.InPort[T]
  type !![T] = io.threadcso.alternation.channel.OutPort[T]

  /** Return a synchronous '''alt-capable''' channel (with the given `name`)
    * designed for point-to-point communication. There is a (weak) dynamic check
    * against multiple processes writing/reading simultaneously.
    */
  def OneOne[T](name: String = null): Chan[T] =
    new io.threadcso.alternation.channel.OneOne[T](name)

  /** Return a synchronous '''alt-capable''' channel (with an
    * automatically-generated `name`) designed for point-to-point communication.
    * There is a (weak) dynamic check against multiple processes writing/reading
    * simultaneously.
    */
  def OneOne[T]: Chan[T] =
    new io.threadcso.alternation.channel.OneOne[T](name = null)

  /** Return a synchronous '''alt-capable''' channel (with the given `name`)
    * designed for shared synchronous communication between `writers` writers,
    * and `readers` readers. When all writers have invoked the method `closeOut`
    * (or all readers the method `closeIn`), the channel closes. If either
    * `writers` or `readers` is nonpositive, then the channel can be closed an
    * unbounded number of times in the associated direction.
    */
  def N2N[T](
      writers: Int,
      readers: Int,
      name: String = null,
      fairOut: Boolean = false,
      fairIn: Boolean = false
  ): SharedAltCapableChannel[T] =
    new io.threadcso.alternation.channel.N2N[T](
      writers,
      readers,
      name,
      fairOut,
      fairIn
    )

  /** Return a buffered '''alt-capable''' channel (with the given `name`)
    * designed for communication between a writer and a reader. This is
    * functionally equivalent to `N2NBuf(size, 1, 1, name)`.
    */
  def OneOneBuf[T](size: Int, name: String = null): Chan[T] =
    new io.threadcso.alternation.channel.OneOneBuf[T](size, name)

  /** Return a buffered '''alt-capable''' channel (with the given `name`)
    * designed for shared communication between `writers` writers, and `readers`
    * readers. When all writers have invoked the method `closeOut` (or all
    * readers the method `closeIn`), the channel closes. If either `writers` or
    * `readers` is nonpositive, then the channel can be closed an unbounded
    * number of times in the associated direction.
    */
  def N2NBuf[T](
      size: Int,
      writers: Int,
      readers: Int,
      name: String = null,
      fairOut: Boolean = false,
      fairIn: Boolean = false
  ): SharedAltCapableChannel[T] =
    new io.threadcso.alternation.channel.N2NBuf[T](
      size,
      writers,
      readers,
      name,
      fairOut,
      fairIn
    )

  /** Abbreviation for N2N(0, 1, name) */
  def ManyOne[T](name: String = null): SharedAltCapableChannel[T] =
    N2N(writers = 0, readers = 1, name)
  def ManyOne[T]: SharedAltCapableChannel[T] = ManyOne[T]()

  /** Abbreviation for N2N(1, 0, name) */
  def OneMany[T](name: String = null): SharedAltCapableChannel[T] =
    N2N(writers = 1, readers = 0, name)
  def OneMany[T]: SharedAltCapableChannel[T] = OneMany[T]()

  /** Abbreviation for N2N(0, 0, name) */
  def ManyMany[T](name: String = null): SharedAltCapableChannel[T] =
    N2N(writers = 0, readers = 0, name)
  def ManyMany[T]: SharedAltCapableChannel[T] = ManyMany[T]()

  /////////////////////////// Notation for guarded events  //////////////////////

  /** This implicit class is used in the implementation of guarded I/O-event
    * notation. The operator `&&` replaced the operator `&&&` in CSO version
    * 1.2.
    *
    * @see
    *   [[io.threadcso.alternation]]
    */

  implicit class Guarded(guard: => Boolean) {
    def &&[T](port: alternation.channel.InPort[T]) =
      new alternation.channel.GuardedInPort[T](() => guard, port)
    def &&[T](port: alternation.channel.OutPort[T]) =
      new alternation.channel.GuardedOutPort[T](() => guard, port)
    def &&[T](chan: alternation.channel.Chan[T]) =
      new alternation.channel.GuardedChan[T](() => guard, chan)
  }

  /** This implicit class provides additional methods that support the legible
    * formatting of the `Nanoseconds` value `_elapsed` (which can be negative).
    */
  implicit class NanoTime(_elapsed: Nanoseconds) {
    private[this] val elapsed = if (_elapsed < 0) -_elapsed else _elapsed
    private[this] val sign = if (_elapsed < 0) "-" else ""
    private[this] val H = elapsed / Hour
    private[this] val M = (elapsed % Hour) / Min
    private[this] val S = (elapsed % Min) / Sec
    private[this] val mS = (elapsed % Sec) / milliSec
    private[this] val muS = (elapsed % milliSec) / microSec

    /** A string representing `_elapsed` in the form `H:M:S.ms,μs` */
    def HMS: String =
      f"$sign%s$H%d:$M%02d:$S%02d.$mS%03d,$muS%03d"

    /** A string representing `_elapsed` in the form `H:M:S.ms,μs` with hours,
      * minutes suppressed if zero.
      */
    def hms: String = {
      val h = if (H == 0) "" else f"$H%d:"
      val m = if (H == 0 && M == 0) "" else f"$M%02d:"
      f"$sign%s$h%s$m%s$S%02d.$mS%03d,$muS%03d"
    }

    /** `(elapsed-startTime).HMS` */
    def Δs: String = (elapsed - startTime).HMS

    /** `(elapsed-startTime).hms` */
    def δs: String = (elapsed - startTime).hms

    /** `(elapsed-startTime).HMS` */
    def Delta: String = (elapsed - startTime).HMS

    /** `(elapsed-startTime).hms` */
    def delta: String = (elapsed - startTime).hms
  }

  /** The value of `nanoTime` when the entire program was started -- useful only
    * for relative timing.
    */
  val startTime: Nanoseconds = nanoTime

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
    * def mycopy[T](in: ?[T], out: ![T]) = proc
    * { var buf = in.nothing
    * var n   = 0
    * val format = new DebuggerFormat(s"\$n: \$buf")
    * format.withDebugger(true)
    * {
    * repeat { buf=in?(); out!buf; n+=1 }
    * }
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
    * def mycopy[T](in: ?[T], out: ![T]) = proc
    * { var buf = in.nothing
    * var n   = 0
    * withDebuggerFormat(s"\$n: \$buf")
    * {
    * repeat { buf=in?(); out!buf; n+=1 }
    * }
    * }
    *
    *
    * }}}
    */
  def withDebuggerFormat[T](theFormat: => String, register: Boolean = true)(
      body: => T
  ): T = {
    val format = new DebuggerFormat(theFormat)
    format.withDebugger(register) { body }
  }

}
