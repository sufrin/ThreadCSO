package io.threadcso.debug

import io.threadcso.basis.getPropElse

// import scala.annotation.elidable
// import scala.annotation.elidable.FINEST

/**
  * <p> A component to support high-resolution logging within the CSO implementation.
  *
  * Methods are effective only when compiled without eliding FINEST.
  *
  * Production versions of the threadCSO jar normally elide these features.
  *
  * Debugging detail is controlled by the integer JVM property `io.threadcso.logging`.
  * its bits are interpreted as boolean switches.
  *
  * The log keeps the most recent `logSize` logging messages, set by the integer JVM
  * property: `io.threadcso.logsize`.
  *
  *{{{@author Bernard Sufrin, Oxford
  *\$Revision: 213 $
  *\$Date: 2017-09-30 16:27:58 +0100 (Sat, 30 Sep 2017) $
  * }}}
  */
//noinspection UnitMethodIsParameterless,UnitMethodIsParameterless
// @elidable(FINEST) 

object Logging
{ // @elidable(FINEST)
  val log = new Logger("Logging", logSize = getPropElse("io.threadcso.logsize", _.toInt)(50),
                                  mask  = getPropElse("io.threadcso.logging", _.toInt)(0))
}
  
