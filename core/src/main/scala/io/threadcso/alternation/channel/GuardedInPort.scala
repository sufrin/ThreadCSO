package io.threadcso.alternation.channel

import io.threadcso.alternation.event.{InPortEvent, InPortEventExtended}

/** Guarded input port syntax */
class GuardedInPort[+T](guard: () => Boolean, port: InPort[T]) {

  /** Construct a boolean-guarded input event for use in an alternation. */
  def =?=>(body: T => Unit) = new InPortEvent(guard, port, body)

  /** Construct a boolean-guarded extended-rendezvous input event for use in an
    * alternation.
    */
  def =??=>(body: T => Unit) = new InPortEventExtended(guard, port, body)
}
