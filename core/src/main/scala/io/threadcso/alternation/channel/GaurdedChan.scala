package io.threadcso.alternation.channel

import io.threadcso.alternation.event.{
  InPortEvent,
  InPortEventExtended,
  OutPortEvent
}

/** Guarded channel syntax */
class GuardedChan[T](guard: () => Boolean, chan: Chan[T]) {
  def =?=>(body: T => Unit) = new InPortEvent(guard, chan.inPort, body)
  def =??=>(body: T => Unit) =
    new InPortEventExtended(guard, chan.inPort, body)
  def =!=>(gen: => T) = new OutPortEvent(guard, chan.outPort, () => gen)
}
