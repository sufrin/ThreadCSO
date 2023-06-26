package io.threadcso.alternation

import java.util.concurrent.atomic.AtomicReference

import io.threadcso.channel._
import io.threadcso.debug.Logging
import io.threadcso.lock.SimpleLock

import scala.annotation._
import scala.annotation.elidable._

/** This package defines transport that can participate in the CSO alternation
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
  import io.threadcso.alternation.event._

  /** The "guard" installed in an unguarded event */
  private[this] val ALWAYS = { () => true }

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
  }

  /** Guarded input port syntax */
  class GuardedInPort[+T](guard: () => Boolean, port: InPort[T]) {

    /** Construct a boolean-guarded input event for use in an alternation. */
    def =?=>(body: T => Unit) = new InPortEvent(guard, port, body)

    /** Construct a boolean-guarded extended-rendezvous input event for use in
      * an alternation.
      */
    def =??=>(body: T => Unit) = new InPortEventExtended(guard, port, body)
  }

  /** Alt-capable output port */
  trait OutPort[-T] extends io.threadcso.channel.OutPort[T] {

    /** Construct an unconditional output event for use in an alternation. */
    def =!=>(gen: => T) = new OutPortEvent(ALWAYS, this, () => gen)
    def registerOut(alt: Runnable, theIndex: Int): PortState
    def unregisterOut(): PortState
  }

  /** Guarded output port syntax */
  class GuardedOutPort[T](guard: () => Boolean, port: OutPort[T]) {

    /** Construct a boolean-guarded output event for use in an alternation. */
    def =!=>(gen: => T) = new OutPortEvent(guard, port, () => gen)
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
  }

  /** Guarded channel syntax */
  class GuardedChan[T](guard: () => Boolean, chan: Chan[T]) {
    def =?=>(body: T => Unit) = new InPortEvent(guard, chan.inPort, body)
    def =??=>(body: T => Unit) =
      new InPortEventExtended(guard, chan.inPort, body)
    def =!=>(gen: => T) = new OutPortEvent(guard, chan.outPort, () => gen)
  }

  trait SharedAltCapableChannel[T]
      extends AltCapableChannel[T]
      with SharedOutPort[T]
      with SharedInPort[T] {}

  //////////////////////// end of Chan  ////////////////////////

  /** <p> Mixin for transport that are capable of participating in alternations.
    * Deals with registration/unregistration of a channel with a running
    * alternation, and with detailed logging (when not elided) of port-state
    * changes. Typical examples of its use can be found in the definitions of
    * channel constructors such as [[io.threadcso.alternation.channel.OneOne]]
    * and [[io.threadcso.alternation.channel.N2NBuf]].
    */

  trait AltCapableChannel[T] extends Chan[T] {

    /** For detailed logging */
    @inline @elidable(FINEST) def logLastReg(
        port: String,
        unreg: Boolean,
        state: PortState
    ): Unit =
      if (state != CLOSEDSTATE)
        Logging.log(
          LoggingFlags.PORTREG,
          s"$name.$port ${if (unreg) "un" else ""}registered when in state $state"
        )

    /** For detailed logging */
    @inline @elidable(FINEST) def logLastInState(state: PortState): Unit =
      Logging.log(LoggingFlags.PORTSTATE, s"$name.InState:=$state")

    /** For detailed logging */
    @inline @elidable(FINEST) def logLastOutState(state: PortState): Unit =
      Logging.log(LoggingFlags.PORTSTATE, s"$name.OutState:=$state")

    /** Mutual exclusion for alternation registration and port-state setting */
    private[this] val mutex = new SimpleLock("channel mutex", this)

    /** The running alternation currently registered with this channel: there
      * can be no more than one.
      */
    private[this] val theAlt = new AtomicReference[Runnable](IgnorePortEvents)

    /** Index of this channel in the currently-registered running alternation */
    protected[this] var theIndex: Int = -1

    /** Register an input event with this channel, and return the port state */
    def registerIn(alt: Runnable, index: Int): PortState = mutex withLock {
      if (!theAlt.compareAndSet(IgnorePortEvents, alt))
        throw new IllegalStateException(
          s"Channel $index cannot participate in more than one alt simultaneously: $alt} and ${theAlt.get}"
        )
      theIndex = index
      val state = inPortState
      logLastReg("inPort", false, state)
      state
    }

    /** Unregister an input event with this channel, and return the port state
      */
    def unregisterIn(): PortState = mutex withLock {
      theAlt.set(IgnorePortEvents)
      theIndex = -1
      val state = inPortState
      logLastReg("inPort", true, state)
      state
    }

    /** Register an output event with this channel, and return the port state */
    def registerOut(alt: Runnable, index: Int): PortState = mutex withLock {
      if (!theAlt.compareAndSet(IgnorePortEvents, alt))
        throw new IllegalStateException(
          s"Channel $index cannot participate in more than one alt simultaneously: $alt} and ${theAlt.get}"
        )
      theIndex = index
      val state = outPortState
      logLastReg("outPort", false, state)
      state
    }

    /** Unregister an output event with this channel, and return the port state
      */
    def unregisterOut(): PortState = mutex withLock {
      theAlt.set(IgnorePortEvents)
      theIndex = -1
      val state = outPortState
      logLastReg("outPort", true, state)
      state
    }

    /** Standard implementation of `setOutPortState` for alt-able channel */
    @inline
    protected def setOutPortStateImp(
        portState: PortState
    ): Unit = // mutex withLock
      {
        logLastOutState(portState)
        if (portState != UNKNOWNSTATE)
          theAlt.get.notifyPortEvent(theIndex, portState)
      }

    /** Standard implementation of `setInPortState` for alt-able channel */
    @inline
    protected def setInPortStateImp(
        portState: PortState
    ): Unit = // mutex withLock
      {
        logLastInState(portState)
        if (portState != UNKNOWNSTATE)
          theAlt.get.notifyPortEvent(theIndex, portState)
      }

    /** For use by the debugger */
    override def showState(out: java.io.PrintWriter): Unit = {
      super.showState(out)
      if (theAlt.get != IgnorePortEvents)
        out.print(s"\n\tREGISTERED AS $theIndex IN ${theAlt.get} ")
    }

  }

}
