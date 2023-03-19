package io.threadcso.alternation

/** This package defines channels that can participate in the CSO alternation
  * constructs.
  *
  * I have separated syntax and semantics as far as was practical, given my
  * reluctance to get embroiled with the Scala macro system (it was unstable
  * when the CSO work started)
  *
  * An [[io.threadcso.alternation.Run]] embodies the state of execution of an
  * alternation.
  *
  * The ''guarded event'' notation: (guard && port) =...=> { .... }` is parsed
  * to a `Guarded...Port =...=> ...` then to an appropriate class of event. The
  * former is achieved by defining the implicit class [[io.threadcso.Guarded]]
  * in the top-level CSO API. This provides the appropriate extension to
  * `Boolean`.
  * {{{
  * @author Bernard Sufrin, Oxford
  * \$Revision: 240 $
  * \$Date: 2017-10-13 18:12:11 +0100 (Fri, 13 Oct 2017) $
  * }}}
  */
package object channel {
  // import io.threadcso.alternation.event._
}
