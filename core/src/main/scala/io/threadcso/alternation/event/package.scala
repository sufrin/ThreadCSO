package io.threadcso.alternation

import io.threadcso.alternation.channel.{InPort, OutPort}
import io.threadcso.basis.Nanoseconds
import io.threadcso.channel.PortState

/** <p> This package implements the syntax of the CSO alternation notations, and
  * provides hooks for the semantics of that notation.
  *
  * The body of an '''alternation''' specification consists of a sequence
  * (possibly empty) of ''executable event'' specifications separated by `|` and
  * possibly followed by: {{{| after(timeout) ==> { ... }}}} or
  * {{{| orelse ==> { ... }}}} or
  * {{{| after(timeout) ==> { ... } |  orelse ==> { ... }}}}
  *
  * An '''executable event specification''' takes one of the following forms:
  * {{{
  * outport              =!=>  OutEvent
  * (guard && outport)   =!=>  OutEvent
  * inport               =?=>  InEvent
  * (guard && inport)    =?=>  InEvent
  * inport               =??=> InEvent     // extended rendezvous form
  * (guard && inport)    =??=> InEvent     // extended rendezvous form
  * }}}
  *
  * Concessions to readability: a `Chan` expression may appear in place of a
  * port expression.
  *
  * ==Events and their effects==
  * {{{
  * OutEvent form:                        Effect when triggered
  * {expr}                                outport!expr
  * {expr} ==> { command: Unit }          {outport!expr; command}
  * }}}
  *
  * {{{
  * InEvent form:                         Effect when triggered
  * { bv => body: Unit }                  {val bv=inport?(); body }
  * }}}
  *
  * For an extended rendezvous `InEvent` the correspondence is
  * {{{
  * { bv => body: Unit }                  { inport??({ bv => body}) }
  * }}}
  */

package object event {

  trait Event {

    /** Recover the events, deadline, and alternative clauses ready for
      * execution as the body of an alternation.
      */
    def normalize(): NormalAlt = {
      val accum = new scala.collection.mutable.Queue[ExecutableEvent]

      def eventsOf(event: Event): Unit = {
        event match {
          case e: OrElseEvent         => eventsOf(e.scope)
          case e: AfterEvent          => eventsOf(e.scope)
          case InfixEventSyntax(l, r) => eventsOf(l); eventsOf(r)
          case ex: ExecutableEvent    => accum enqueue ex
          case null                   => {}
        }
      }

      eventsOf(this)
      NormalAlt(accum.toArray[ExecutableEvent], afterOf(this), orElseOf(this))
    }

    def toNormalizedString: String = normalize().toString()
  }

  /** Represents the "compiled" body of an alternation */
  case class NormalAlt(
      events: collection.Seq[ExecutableEvent],
      after: AfterEvent,
      orelse: OrElseEvent
  )

  private def afterOf(event: Event): AfterEvent = {
    event match {
      case e: AfterEvent  => e
      case e: OrElseEvent => afterOf(e.scope)
      case _              => null
    }
  }

  private def orElseOf(event: Event): OrElseEvent = {

    event match {
      case e: OrElseEvent => e
      case e: AfterEvent  => orElseOf(e.scope)
      case _              => null
    }
  }

  ////////////////////////////////////////

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

  /** `ExecutableEvent`s are composed ''syntactically'' by infix `|` and its
    * prefix form.
    */
  trait ExecutableEventSyntax extends Event {
    def |(other: ExecutableEventSyntax) = InfixEventSyntax(this, other)

    def |(other: AfterEvent): AfterEvent = other.withScope(this)

    def |(other: OrElseEvent): OrElseEvent = other.withScope(this)
  }

  /** Start of the '''orelse''' notation. */
  object orelse {
    def ==>(body: => Unit) = OrElseEvent(() => body)
  }

  /** Syntactic continuation of '''after'''`(deadline)` notation. */
  class AfterDeadline(deadline: () => Nanoseconds) {
    def ==>(body: => Unit) = AfterEvent(deadline, () => body)
  }

  /** Start of '''after'''`(deadline)` notation (unit is nanoseconds). */
  def after(deadline: => Nanoseconds) = new AfterDeadline(() => deadline)

  /** An `orelse` "event" */
  case class OrElseEvent(body: () => Unit) extends Event {
    var scope: Event = _

    def withScope(event: Event): OrElseEvent = {
      scope = event; this
    }

    override def toString = "| orelse==>(.)"
  }

  /** An `after` "event" */
  case class AfterEvent(deadline: () => Long, body: () => Unit) extends Event {
    var scope: Event = _

    def withScope(event: Event): AfterEvent = {
      scope = event; this
    }

    def |(other: OrElseEvent): OrElseEvent = other.withScope(this)

    override def toString = "| after(.)==>."
  }

  /** Syntactic composition of events `l` and `r`. (These are not __really__
    * executable, but life's too short to find a way to make the Scala type
    * system do what amounts to a complete syntax check.)
    */
  case class InfixEventSyntax(l: Event, r: Event)
      extends ExecutableEventSyntax {
    override def toString: String = "%s | %s".format(l, r)
  }

  private[this] def mkInfix(
      l: ExecutableEventSyntax,
      r: ExecutableEventSyntax
  ): ExecutableEventSyntax =
    InfixEventSyntax(l, r)

  /** Prefix notation for `|`: `|(e,,1,,, ... e,,n,,)` = `e,,1,, | ... | e,,n,,`
    */
  def |(events: collection.Seq[ExecutableEvent]): ExecutableEventSyntax =
    events.reduce(mkInfix)

  //////////// Executable events //////////////

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

  /*
  /** This was going to support outport events of the form `=!=> { ... value } ==> { bv => body }` where
   * the value output to the port is passed as a parameter to the continuation. Unfortunately
   * I have been unable to construct a `==>`-like operator in `OutPortEvent` with an appropriate type for
   * the `cont`inuation without sacrificing the contravariance of `OutPort`. It's relatively
   * straightforward to program around this so I didn't try very hard.
   */
  class OutPortEventThenWithOutput[-T](guard: () => Boolean, port: OutPort[T], gen: ()=>T, cont: T=>Unit)
        extends ExecutableEvent
  { protected [alternation]
    def isOpen: Boolean = port.canOutput
    protected [alternation]
    def isFeasible: Boolean = guard() && isOpen
    def run(): Unit =  { val v = gen(); port ! v; cont(v) }
    def register(alt: Runnable, theIndex: Int): PortState = {port.registerOut(alt, theIndex)  }
    def unregister: PortState = {port.unregisterOut() }
    def portState: PortState = port.outPortState

    override def toString = s"•$port=!=>•==>{_=>•}"
  }
   */
}
