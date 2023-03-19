package io.threadcso.alternation.event

import io.threadcso.alternation.event.EventHelpers._
import io.threadcso.alternation.channel.{InPort, OutPort}
// import io.threadcso.alternation.Runnable
import io.threadcso.channel.PortState

/** `ExecutableEvent`s are composed ''syntactically'' by infix `|` and its
  * prefix form.
  */
sealed trait ExecutableEventSyntax extends Event {
  def |(other: ExecutableEventSyntax) = InfixEventSyntax(this, other)

  def |(other: AfterEvent): AfterEvent = other.withScope(this)

  def |(other: OrElseEvent): OrElseEvent = other.withScope(this)
}

/** Syntactic composition of events `l` and `r`. (These are not __really__
  * executable, but life's too short to find a way to make the Scala type system
  * do what amounts to a complete syntax check.)
  */
case class InfixEventSyntax(l: Event, r: Event) extends ExecutableEventSyntax {
  override def toString: String = "%s | %s".format(l, r)
}

/** <p> The meaning of an inport or outport events is represented by a
  * (concrete) `ExecutableEvent`. Alternation constructs work by choosing an
  * executable event that is ready for execution, then running it.
  */

abstract case class ExecutableEvent() extends ExecutableEventSyntax {

  /** Is the associated port open */
  def isOpen: Boolean

  /** Is the associated guard true and the associated port open */
  def isFeasible: Boolean

  /** Run the event */
  def run(): Unit

  /** Register the given running alternation with this event's port and return
    * the current state of the port
    */
  def register(alt: Runnable, theIndex: Int): PortState

  /** Unregister any running alternation from this event's port and return the
    * current state of the port.
    */
  def unregister: PortState

  /** Return the current state of this event's port */
  def portState: PortState

}

/** Executable event corresponding to `guard && port =?=> body` */
class InPortEvent[+T](guard: () => Boolean, port: InPort[T], body: T => Unit)
    extends ExecutableEvent {
  def isOpen: Boolean = port.canInput

  def isFeasible: Boolean = guard() && isOpen

  def run(): Unit = {
    body(port ? ())
  }

  def register(alt: Runnable, theIndex: Int): PortState = {
    port.registerIn(alt, theIndex)
  }

  def unregister: PortState = {
    port.unregisterIn()
  }

  def portState: PortState = port.inPortState

  override def toString = s"•${port.name}=?=>•"
}

/** Executable event corresponding to `guard && port =??=> body` */
class InPortEventExtended[+T](
    guard: () => Boolean,
    port: InPort[T],
    body: T => Unit
) extends ExecutableEvent {
  def isOpen: Boolean = port.canInput

  def isFeasible: Boolean = guard() && isOpen

  def run(): Unit = {
    port ? body
  }

  def register(alt: Runnable, theIndex: Int): PortState = {
    port.registerIn(alt, theIndex)
  }

  def unregister: PortState = {
    port.unregisterIn()
  }

  def portState: PortState = port.inPortState

  override def toString = s"•${port.name}=??=>•"
}

/** Executable event corresponding to `guard && port =!=> body` */
class OutPortEvent[-T](guard: () => Boolean, port: OutPort[T], gen: () => T)
    extends ExecutableEvent {
  def isOpen: Boolean = port.canOutput

  def isFeasible: Boolean = guard() && isOpen

  def run(): Unit = {
    port ! gen()
  }

  def register(alt: Runnable, theIndex: Int): PortState = {
    port.registerOut(alt, theIndex)
  }

  def unregister: PortState = {
    port.unregisterOut()
  }

  def portState: PortState = port.outPortState

  def ==>(cont: => Unit) = new OutPortEventThen(guard, port, gen, () => cont)

  override def toString = s"•${port.name}=!=>•"
}

/** Executable event corresponding to `guard && port =!=> body ==> cont` */
class OutPortEventThen[-T](
    guard: () => Boolean,
    port: OutPort[T],
    gen: () => T,
    cont: () => Unit
) extends ExecutableEvent {
  def isOpen: Boolean = port.canOutput

  def isFeasible: Boolean = guard() && isOpen

  def run(): Unit = {
    port ! gen(); cont()
  }

  def register(alt: Runnable, theIndex: Int): PortState = {
    port.registerOut(alt, theIndex)
  }

  def unregister: PortState = {
    port.unregisterOut()
  }

  def portState: PortState = port.outPortState

  override def toString = s"•${port.name}=!=>•==>•"
}
