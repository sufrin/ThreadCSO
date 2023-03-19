package io.threadcso.alternation.channel

import java.util.concurrent.atomic.AtomicReference

import io.threadcso.channel._
import io.threadcso.debug.Logging
import io.threadcso.lock.SimpleLock
import io.threadcso.alternation.{LoggingFlags, IgnorePortEvents, Runnable}

import scala.annotation.elidable
import scala.annotation.elidable.FINEST

/** <p> Mixin for channels that are capable of participating in alternations.
  * Deals with registration/unregistration of a channel with a running
  * alternation, and with detailed logging (when not elided) of port-state
  * changes.
  *
  * Typical examples of its use can be found in the definitions of channel
  * constructors such as [[io.threadcso.alternation.channel.OneOne]] and
  * [[io.threadcso.alternation.channel.N2NBuf]].
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

  /** The running alternation currently registered with this channel: there can
    * be no more than one.
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
