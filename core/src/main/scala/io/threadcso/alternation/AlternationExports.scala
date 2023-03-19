package io.threadcso.alternation

trait AlternationExports {
  type Run = io.threadcso.alternation.Run

  import event._
  import io.threadcso.process.Stopped
  import io.threadcso.basis.Nanoseconds

  import io.SourceLocation.SourceLocation

  // macro definition needs to be enabled
  import scala.language.experimental.macros

  /** Provides the source location to pass as `(implicit loc: SourceLocation)`
    * parameters
    */
  implicit def sourceLocation: SourceLocation =
    macro io.SourceLocation.sourceLocationMACRO

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
    event.EventHelpers.after _

  /** orelse syntax for 'alt' bodies */
  val orelse: event.orelse.type =
    event.orelse

  /** prefix | syntax for 'alt' bodies */
  val | : (collection.Seq[ExecutableEvent]) => ExecutableEventSyntax =
    event.EventHelpers.|

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

  /** `repeatFor (it: Iterable[T]) { bv => body }` applies the function `{ by =>
    * body }` to each of the elements of an iterator formed from the Iterable.
    * If an exception ''ex'' is thrown, then stop the iteration, then unless
    * ''ex'' is a `Stopped` re-throw ''ex''.
    */

  @inline def repeatFor[T](it: Iterable[T])(fn: T => Unit): Unit =
    attempt { for (t <- it) fn(t) } {}

  /** Returns true iff the current thread has been cancelled, and clears the
    * cancelled bit in the thread if so.
    */
  def interrupted(): Boolean = java.lang.Thread.interrupted()
}
