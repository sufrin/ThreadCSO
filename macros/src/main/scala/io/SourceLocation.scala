package io

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object SourceLocation {

  /** Source coordinates of the position at which this macro is invoked. One
    * scenario in which it might be used is:
    *
    * {{{
    *     abstract class AltTrial(implicit loc: SourceLocation)
    *     {
    *      ...
    *     }
    * }}}
    *
    * Whenever this class is used to extend another class, the source
    * coordinates of the location of the extension are made available.
    *
    * Another scenario is exemplified by the signature of
    * [[io.threadcso.debug.Logger.log]]:
    *
    * {{{def log(bits: Int, text: => String)(implicit loc: SourceLocation)}}}
    *
    * This method records in a log (among other things) the location in the
    * source text of its own call.
    *
    * @see
    *   [[io.SourcePath]]
    */

  implicit def sourceLocation: SourceLocation = macro sourceLocationMACRO

  /** Source location coordinates */
  case class SourceLocation(file: String, line: Int, offset: Int) {
    override def toString = s"$file:$line.$offset"
  }

  def sourceLocationMACRO(c: Context): c.universe.Tree = {
    import c.universe._

    val pos = c.enclosingPosition
    val filename = pos.source.file.name
    val line = pos.line
    val charOffset = pos.column
    q"""io.SourceLocation.SourceLocation($filename, $line, $charOffset)"""
  }

  def sourcePathMACRO(c: Context): c.universe.Tree = {
    import c.universe._
    val pos = c.enclosingPosition
    val fileURI =
      try { pos.source.file.file.toPath.toString }
      catch { case _: NullPointerException => pos.source.file.name }
    val line = pos.line
    val charOffset = pos.column
    q"""io.SourceLocation.SourceLocation($fileURI, $line, $charOffset)"""
  }

}
