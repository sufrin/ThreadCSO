package io.threadcso.alternation.channel

import io.threadcso.alternation.event.{
  InPortEvent,
  InPortEventExtended,
  OutPortEvent
}

/** A `Chan` embodies an `InPort` and an `OutPort`. If `chan: Chan` then the
  * alternation notations `chan ... =?=> ...`, `chan ... =??=> ...` and `chan
  * ... =!=>...` are available for direct use in an '''alt'''.
  */
trait Chan[T]
    extends io.threadcso.channel.Chan[T]
    with InPort[T]
    with OutPort[T] {

  /** `this` as an `InPort` */
  val inPort: InPort[T] = this

  /** `this` as an `OutPort` */
  val outPort: OutPort[T] = this

  override def =?=>(body: T => Unit) = new InPortEvent(ALWAYS, this, body)

  override def =??=>(body: T => Unit) =
    new InPortEventExtended(ALWAYS, this, body)

  override def =!=>(gen: => T) = new OutPortEvent(ALWAYS, this, () => gen)

  private[this] val ALWAYS = { () => true }
}
