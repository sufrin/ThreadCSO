package io.threadcso.alternation.event

import io.threadcso.basis.Nanoseconds

trait Event {
  import EventHelpers._

  /** Recover the events, deadline, and alternative clauses ready for execution
    * as the body of an alternation.
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
}

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

/** Start of the '''orelse''' notation. */
object orelse {
  def ==>(body: => Unit) = OrElseEvent(() => body)
}

/** Syntactic continuation of '''after'''`(deadline)` notation. */
class AfterDeadline(deadline: () => Nanoseconds) {
  def ==>(body: => Unit) = AfterEvent(deadline, () => body)
}

/** Represents the "compiled" body of an alternation */
case class NormalAlt(
    events: collection.Seq[ExecutableEvent],
    after: AfterEvent,
    orelse: OrElseEvent
)
