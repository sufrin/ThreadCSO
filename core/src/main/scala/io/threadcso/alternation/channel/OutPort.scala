package io.threadcso.alternation.channel

import io.threadcso.channel.PortState
import io.threadcso.alternation.Runnable
import io.threadcso.alternation.event.{OutPortEvent}

/** Alt-capable output port */
trait OutPort[-T] extends io.threadcso.channel.OutPort[T] {

  /** Construct an unconditional output event for use in an alternation. */
  def =!=>(gen: => T) = new OutPortEvent(ALWAYS, this, () => gen)
  def registerOut(alt: Runnable, theIndex: Int): PortState
  def unregisterOut(): PortState

  private[this] val ALWAYS = { () => true }
}
