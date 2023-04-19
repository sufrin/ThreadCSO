package io

import io.SourceLocation.{sourcePathMACRO, sourceLocationMACRO}

import scala.language.experimental.macros

/** Same functionality as `SourceLocation`
  *
  * @see
  *   [[io.SourceLocation]]
  */
object SourcePath {

  /** Source coordinates of the position at which this macro is invoked, with
    * file location expressed as a complete path unless there is no URI
    * associated with the position, in which case the result is the same as
    * `SourceLocation.sourceLocation` would return.
    */

  implicit def sourcePath:     SourcePath     = macro sourcePathMACRO
  implicit def sourceLocation: SourceLocation = macro sourceLocationMACRO
  type SourceLocation = io.SourceLocation.SourceLocation
  type SourcePath     = io.SourceLocation.SourcePath

}
