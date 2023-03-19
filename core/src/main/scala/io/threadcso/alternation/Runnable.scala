package io.threadcso.alternation

import io.threadcso.channel.PortState

/** Interface between (the abstract syntax of) events and the specific
  * implementations of `alt` and `serve` (etc) used in a CSO implementation.
  * {{{
  * @author Bernard Sufrin, Oxford
  * $Revision: 212 $
  * $Date: 2017-09-29 18:07:56 +0100 (Fri, 29 Sep 2017) $
  * }}}
  */
trait Runnable {

  /** <p>Invoked by the event numbered `theIndex` in the running alternation
    * when the state of its port changes to `portState`, in order to notify the
    * running alternation that a port has changed state. In the present
    * implementation its parameters are ignored.
    */
  def notifyPortEvent(theIndex: Int, portState: PortState): Unit
}

/** A `Runnable` that ignores port event notifications. */
object IgnorePortEvents extends Runnable {
  def notifyPortEvent(theIndex: Int, portState: PortState): Unit = {}
}
