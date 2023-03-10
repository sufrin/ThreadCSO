package io

import scala.reflect.macros.blackbox
import scala.language.experimental.macros

/** Provides macro implementations of the stock control structures `attempt`,
  * and `repeat` for use in contexts where maximum performance is required. The
  * inlining of the stock methods is nearly always good enough.
  *
  * @since 1.1R4
  */
object control {

  /** Evaluate `body` and return its value unless an exception ''ex'' is thrown.
    * If ''ex'' is a `Stopped` then evaluate and return the value of `alt`,
    * otherwise re-throw ''ex''.
    */
  def ATTEMPT[T](body: T)(alt: T): T = macro ATTEMPTMACRO
  def ATTEMPTMACRO(c: blackbox.Context)(body: c.Tree)(alt: c.Tree): c.Tree = {
    import c.universe._
    q"""
      try   { $body }
      catch { case s: io.threadcso.process.Stopped   => $alt
              case t: Throwable => throw t
            }
    """
  }

  /** Iterate `body` while the evaluation of `guard` yields `true`. If an
    * exception ''ex'' is thrown, then stop the iteration, then unless ''ex'' is
    * a `Stopped` re-throw ''ex''.
    */
  def REPEAT(guard: Boolean)(body: Unit): Unit = macro REPEATMACRO
  def REPEATMACRO(
      c: blackbox.Context
  )(guard: c.Tree)(body: c.Tree): c.universe.Tree = {
    import c.universe._
    q"""
    {   var go = $guard
        while (go)
        try   { $body; go = $guard }
        catch { case s: io.threadcso.process.Stopped => go = false
                case t: Throwable => throw t
              }
    }
    """
  }

  /** Iterate `body`. If an exception ''ex'' is thrown, then stop the iteration,
    * and unless ''ex'' is a `Stopped` re-throw ''ex''.
    */
  def REPEAT(body: Unit): Unit = macro UNGUARDEDREPEATMACRO
  def UNGUARDEDREPEATMACRO(
      c: blackbox.Context
  )(body: c.Tree): c.universe.Tree = {
    import c.universe._
    q"""
    {
        try   { while (true) { $body } }
        catch { case s: io.threadcso.process.Stopped => {}
                case t: Throwable => throw t
              }
    }
    """
  }

}
