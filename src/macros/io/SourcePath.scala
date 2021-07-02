package io

import io.SourceLocation.sourcePathMACRO
import scala.language.experimental.macros

/**
  *
  * Import all from this object to enable source location reporting as complete paths instead of filenames
  *
  * @see [[io.SourceLocation]]
  */
object SourcePath {
  /**
    * Source coordinates of the position at which this macro is invoked, with file location expressed as a complete path
    * unless there is no URI associated with the position, in which case the result is the same as `SourceLocation.sourceLocation`
    * would return.
    *
    **/

  type SourceLocation = io.SourceLocation.SourceLocation

  implicit def sourceLocation: SourceLocation = macro sourcePathMACRO
}