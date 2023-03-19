package io.threadcso.alternation.channel

import io.threadcso.channel.PortState
import io.threadcso.alternation.event.{InPortEvent, InPortEventExtended}

/** Alt-capable input port */
trait InPort[+T] extends io.threadcso.channel.InPort[T] {

  /** Construct an unconditional input event for use in an alternation. */
  def =?=>(body: T => Unit) = new InPortEvent(ALWAYS, this, body)

  /** Construct an unconditional extended-rendezvous input event for use in an
    * alternation.
    */
  def =??=>(body: T => Unit) = new InPortEventExtended(ALWAYS, this, body)
  def registerIn(alt: Runnable, theIndex: Int): PortState
  def unregisterIn(): PortState

  /** The "guard" installed in an unguarded event */
  private[this] val ALWAYS = { () => true }
}
