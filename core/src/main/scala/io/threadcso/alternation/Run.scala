package io.threadcso.alternation

import java.util.concurrent.atomic.AtomicInteger

import io.SourceLocation._
import io.threadcso.alternation.event._
import io.threadcso.channel.{CLOSEDSTATE, PortState, READYSTATE, UNKNOWNSTATE}
import io.threadcso.debug.{Logging, REGISTRY}
import io.threadcso.nanoTime
import io.threadcso.process._

import scala.annotation._
import scala.annotation.elidable._

/** The `Stopped` exception raised by an `alt/prialt` with no `orelse` clause
  * when it is has run out of feasible events that might become ready.
  */

case object AltAbort extends Stopped()

/** Manages the state of a running alternation construct whose abstract syntax
  * is `syntax`. If `registerWithDebugger` is `true` then the alternation
  * construct is registered with the debugger ''while it runs''. This is for
  * ''in-extremis'' debugging of deadlocking CSO code that includes
  * alternations.
  *
  * Alternations happen in four phases (these phases are almost the same for
  * '''alt''', '''prialt''', '''serve''' and '''priserve''' constructs), namely:
  * '''Registration''', '''Waiting''', '''Unregistration''', and
  * '''Execution'''.
  *
  * \I. '''Registration''': The current feasibility of all participating events
  * is recorded as `UNKNOWNSTATE`, `INFEASIBLE`, or `READYSTATE`, and their
  * associated ports are marked as being involved in ''this'' running
  * alternation construct. An event for a port that is closed at this stage will
  * never become feasible. An event is infeasible if its associated port has
  * been closed, or if its associated guard is false.
  *
  * \I. '''Waiting''': If, after registration, no `READYSTATE` event is present,
  * but some events are still feasible, then the alternation will wait (with the
  * appropriate deadline, if there's an '''after''' event; and ''sine-die''
  * otherwise) for a port to change state. If there's an '''after''' event and
  * the wait timed-out, then the after event is marked to be executed. If no
  * events are feasible by this phase, or if the last feasible event becomes
  * infeasible as a consequence of a state change (because its port closed
  * during the wait), then the '''orelse''' path is taken, to wit: the
  * '''orelse''' event (if any) is marked to be executed.
  *
  * \I. '''Unregistration''': All ports of all feasible events are now
  * unregistered from their participation in this running alt. If during this
  * phase any already-feasible events are found to be ready then one of them is
  * chosen and marked for execution. The precise method of choice depends on
  * whether or not the alternation construct is supposed to be fair. In this
  * implementation all but '''serve''' alternations choose the first event (in
  * order of appearance in the alternation) found to be ready during
  * unregistration. (''Unregistration of feasible events actually takes place in
  * reverse order of appearance in the alternation, giving "higher priority"
  * feasible events a little more time to become ready.'')
  *
  * \I. '''Execution''': When all event ports have been unregistered, then:
  * \- If a feasible event has been marked for execution, then it is executed;
  * or </li>
  * \- If '''after''' has been marked for execution, then it is executed; or
  * </li>
  * \- If '''orelse''' has been marked for execution, then it is executed (and
  * if the executing alternative is a serve, the serve is terminated)</li>
  * \- If nothing has been marked for execution, then an '''alt''' or
  * '''prialt''' will abort; and a '''serve''' or '''priserve''' will terminate.
  * </li>
  *
  * The present implementation is highly unadventurous and may be less than
  * optimally efficient, though it is (intended to be) scrutable. My experience
  * has been that alternation is tricky to implement efficiently without
  * introducing race conditions that are hard to track down.
  *
  * Possible improvements/speedups include:
  *
  * \- Run the first event found to be ready during registration in an
  * `priserve` or `prialt`.</li>
  * \- Give the programmer more control of the algorithm that chooses ''fairly''
  * between events run during the execution of a `serve`. The `round-robin`
  * method is not always effective -- particularly when certain patterns of
  * guarded event are present in the alternation.
  * \- Distinguish between `alt` and `prialt` implementation by using an
  * ''efficient'' source of ''randomness'' to choose among events that become
  * ready before unregistration.
  *
  * {{{
  * @author Bernard Sufrin, Oxford,
  * \$Revision: 316 $,
  * \$Date: 2018-01-11 18:18:45 +0000 (Thu, 11 Jan 2018) $
  * }}}
  *
  * @param register
  *   Redundant in the current implementation.
  * @param syntax
  *   Abstract syntax of the alternation
  * @param loc
  *   Source loc'n of the alternation (implicit SourceLocation parameter to
  *   alt/prialt/serve/priserve)
  */

sealed class Run(register: Boolean, syntax: Event, loc: SourceLocation)
    extends Runnable
    with REGISTRY.Debuggable {

  import io.threadcso.alternation.LoggingFlags._
  import io.threadcso.debug.Logging._

  /** Thread running this alternation construct: for the debugger */
  private[this] val runner = Thread.currentThread.getName

  /** Syntactic kind of the construct */
  private[this] var altkind = "alt"

  override def toString = toStringConstant

  lazy private val toStringConstant: String = {
    val events = ioEvent.mkString(" | ")
    val aft = if (afterEvent == null) "" else afterEvent.toString
    val orels = if (orelseEvent == null) "" else orelseEvent.toString
    s"""$loc $altkind($events$aft$orels)"""
  }

  /** Invoked by the debugger to show the current state of this running
    * alternation.
    */
  override def showState(out: java.io.PrintWriter): Unit = {
    @inline def printEvent(i: Int) = {
      if (eventFeasible(i)) {
        if (i != 0) out.print("|")
        out.print(getEventState(i).toStateString)
        out.print(ioEvent(i))
      }
    }
    out.print(s"$loc $altkind[${readyCount}R/${feasibleCount}F]")
    out.print("(")
    for (i <- 0 until events) printEvent(i)
    if (afterEvent != null) out.print(afterEvent)
    if (orelseEvent != null) out.print(orelseEvent)
    out.print(s") (in $runner)")
  }

  @inline @elidable(FINEST) private def logRegStart(): Unit =
    Logging.log(REG, s"registration started: $this")

  @inline @elidable(FINEST) private def logRegEnd(): Unit =
    Logging.log(REG, s"registration ended: $this R$readyCount/F$feasibleCount")

  /** Returns `List(runner)` only if the alternation is waiting for an event to
    * become ready; otherwise `List()`.
    */
  override def getWaiting: collection.Seq[Thread] = poll.getWaiting

  /** Runnable alt waits on `poll` when there are feasible events but no ready
    * events
    */
  private[this] val poll = // new io.threadcso.semaphore.jdk.CountingSemaphore (0, "Run", true, false)
    new io.threadcso.semaphore.CountingSemaphore(
      0,
      "AltRun",
      fair = false,
      parent = this,
      spin = 0
    )

  /* NB: September 2017
        The definition of poll as the threadcso rather than the jdk semaphore makes it unnecessary
        to register with the debugger, for when an active alternation thread is
        awaiting the semaphore, its state is available to the debugger because it is
        recorded as the component that called for the thread to be suspended.

        If the threadcso semaphore proves to be unreliable (!) its construction can be replaced by
        // io.threadcso.semaphore.jdk.CountingSemaphore (0,"Run", true, false)
   */

  /** Normalised syntax of the alt body: ''i/o event sequence, after event,
    * orelse event''
    */
  private[this] val NormalAlt(ioEvent, afterEvent, orelseEvent) =
    syntax.normalize()

  private[this] val events = ioEvent.size

  /** Number of events known to be ready: `<=feasibleCount<=events` */
  private[this] val readyCount = new AtomicInteger(0)

  /** Number of events known to be feasible (port open and guard true): `<=
    * events`
    */
  private[this] val feasibleCount = new AtomicInteger(0)

  @inline private[this] def getEventState(i: Int): PortState =
    ioEvent(i).portState

  /** Runnable alternation's record of the feasibility of its events. Determined
    * during registration.
    */
  private[this] val eventFeasible = Array.ofDim[Boolean](events)

  /** Yields readable form of the readinesses of the individual events */
  private[this] def getReadiness = {
    val s = new scala.collection.mutable.StringBuilder
    for (i <- 0 until events)
      if (eventFeasible(i)) { s.append(s"$i: ${getEventState(i)} | ") }
    s.toString
  }

  /** Count the events that are ready, feasible, and (because their port is
    * closed) infeasible
    */
  @inline private[this] def reCount(): Unit = {
    readyCount.set(0)
    feasibleCount.set(0)
    for (i <- 0 until events)
      if (eventFeasible(i))
        getEventState(i) match {
          case UNKNOWNSTATE => feasibleCount.incrementAndGet()
          case READYSTATE =>
            readyCount.incrementAndGet(); feasibleCount.incrementAndGet()
          case CLOSEDSTATE => eventFeasible(i) = false
        }
  }

  /** Take the ''nothing feasible'' branch if it exists, otherwise stop: OrElse
    * in an alt
    */
  private[this] def doOrElse(): Unit = {
    for (event <- ioEvent) event.unregister
    if (orelseEvent != null) {
      orelseEvent.body(); log(RUN, s"ran orelse event: $this")
    } else
      throw AltAbort
  }

  /** Take the ''nothing feasible'' branch if it exists, then stop: OrElse in a
    * serve
    */
  private[this] def doOrElseAndStop(): Unit = {
    for (event <- ioEvent) event.unregister
    if (orelseEvent != null) {
      orelseEvent.body(); log(RUN, s"ran orelse: $this")
    }
    throw AltAbort
  }

  /** Take the ''timed-out'' branch if it exists */
  private[this] def doAfter(): Unit = {
    for (event <- ioEvent) event.unregister
    log(RUN, "about to run after event")
    if (afterEvent == null) throw AltAbort
    else { afterEvent.body(); log(RUN, s"ran after event: $this") }
  }

  /** Find and run a single ready event; unregistering all events. The event
    * selected will be that with the least index, and this is just what is
    * needed for '''prialt''' and '''priserve'''. Infeasible event are not
    * unregistered because they were never registered. A single-shot '''alt'''
    * cannot pretend to be fair, so here we treat it exactly as for
    * '''prialt'''. An alternative for '''alt''' would be to make a random
    * choice from the ready events; but finding an effective and efficient
    * source of randomness can be problematic.
    */
  @inline private[this] def findAndRun(): Unit = {
    assert(readyCount.get > 0)
    log(UNREG, s"FINDANDRUN started: $this R$readyCount/F$feasibleCount")
    var selected = -1
    var index = events

    do {
      index -= 1
      if (eventFeasible(index)) {
        val event = ioEvent(index)
        val unreg = event.unregister
        unreg match {
          case READYSTATE =>
            selected = index
            log(
              UNREG,
              s"READY $event @$selected selected during unregistration"
            )
          case CLOSEDSTATE =>
            log(UNREG, s"CLOSED $event @$index during unregistration")
          case UNKNOWNSTATE =>
            log(UNREG, s"UNK $event @$index during unregistration")
        }
      }
    } while (index > 0)

    log(SEL, s"FINDANDRUN selected $selected")
    // Some ports may have closed during the unregistering
    if (selected >= 0) {
      ioEvent(selected).run()
      log(RUN, s"alt finished running event $selected ${ioEvent(selected)}")
    } else { // noinspection NameBooleanParameters
      // TODO: this was reported a small number of times
      if (readyCount.get > 0) {
        val message =
          (s"$toString with R$readyCount/F$feasibleCount NO SELECTION $getReadiness (channel may have closed while alternation believes it ready)")
        log(UNREG, message)
        throw new IllegalStateException(message)
      }
      doOrElse()
    }
  }

  /** The next place to look in a round-robin search for a ready event */
  private[this] var fairnessOffset = 0

  @inline private[this] def fair(ix: Int): Int = (fairnessOffset + ix) % events

  /** Find and run a single ready event; unregistering all events. The search
    * for the ready event proceeds round-robin from the current
    * ''fairnessOffset'', which always starts at the last selection event.
    * Infeasible event are not unregistered because they were never registered.
    */
  @inline private[this] def findFairlyAndRun(): Unit = {
    assert(readyCount.get > 0)
    log(
      UNREG,
      s"FINDFAIRLYANDRUN started at offset $fairnessOffset R$readyCount/F$feasibleCount"
    )
    var selected = -1
    var index = events
    do {
      index -= 1
      val findex = fair(index)
      if (eventFeasible(findex)) {
        val event = ioEvent(findex)
        val unreg = event.unregister
        unreg match {
          case READYSTATE => {
            selected = findex
            log(UNREG, s"READY $event @$selected during UNREG")
          }
          case CLOSEDSTATE => {
            log(UNREG, s"CLOSED $event @$index during UNREG")
          }
          case UNKNOWNSTATE => {
            log(UNREG, s"UNK $event @$index during UNREG")
          }
        }
      }
    } while (index > 0)

    // Some ports may have closed during the unregistering
    log(SEL, s"FINDFAIRLYANDRUN selected $selected")
    if (selected >= 0) {
      fairnessOffset = (events + selected - 1) % events
      ioEvent(selected).run()
      log(RUN, s"serve finished running event $selected ${ioEvent(selected)}")
    } else { // noinspection NameBooleanParameters
      // TODO: this was reported a handful of times
      if (readyCount.get > 0) {
        val message =
          (s"$toString with R$readyCount/F$feasibleCount NO SELECTION $getReadiness (channel may have closed while alternation believes it ready)")
        log(UNREG, message)
        throw new IllegalStateException(message)
      }
      doOrElse()
    }
  }

  @inline private[this] def awaitSomething(_wait: Long) = {
    /* @elidable(FINEST) */
    var cycles = 0
    @elidable(FINEST) def cycle(): Unit = { cycles += 1 }
    @elidable(FINEST) def logCycles(): Unit = {
      if (cycles > 0)
        log(WAIT, s"Waited ($cycles cycles): R$readyCount/F$feasibleCount")
    }

    if (_wait > 0) {
      val deadline: Long = _wait + nanoTime
      var wait = _wait

      // wait for something to happen or for the deadline to elapse
      while (feasibleCount.get > 0 && readyCount.get == 0 && wait >= 0) {
        log(WAIT, s"Waiting ($wait ns) for poll:  R$readyCount/F$feasibleCount")
        if (poll.tryAcquire(wait)) {
          cycle()
          reCount()
          wait = deadline - nanoTime
        } else {
          wait = -1
        }
      }
    } else {
      while (feasibleCount.get > 0 && readyCount.get == 0) {
        cycle()
        log(
          WAIT,
          s"Waiting for poll: R$readyCount/F$feasibleCount $getReadiness"
        )
        poll.acquire()
        reCount()
      }
    }
    logCycles()

  }

  /** Notification that its state of readiness changed: sent by a port
    * referenced by the `theIndex`th event of this alternation. Only
    * `CLOSEDSTATE` and `READYSTATE` are ever notified by channel
    * implementations, because only these are of interest to an already-running
    * alternation.
    */
  def notifyPortEvent(theIndex: Int, state: PortState): Unit = {
    log(NOTIFY, s"Port State ($theIndex) := $state IN $this")
    poll.release()
  }

  @inline private[this] def registerReadiness(): Unit = {
    readyCount.set(0)
    feasibleCount.set(0)
    logRegStart()
    // the enabling phase
    var i = 0
    for (event <- ioEvent) {
      if (event.isFeasible) {
        val r = event.register(this, i)
        r match {
          case READYSTATE =>
            eventFeasible(i) = true; feasibleCount.incrementAndGet();
            readyCount.incrementAndGet()
          case UNKNOWNSTATE =>
            eventFeasible(i) = true; feasibleCount.incrementAndGet()
          case CLOSEDSTATE =>
            eventFeasible(i) = false;
            log(REG, s"$i : $event closed during registration")
        }
      } else
        eventFeasible(i) = false
      i += 1
    }
    logRegEnd()
  }

  /** Run this alternation as a  '''prialt''' */
  @inline def prialt(): Unit = alt()

  /** Run this alternation as an '''alt''' */
  def alt(): Unit = // withDebugger (registerWithDebugger)
    { // remaining time to wait before timeout
      val wait = if (afterEvent == null) 0L else afterEvent.deadline()
      altkind = "alt"

      registerReadiness()

      awaitSomething(wait)

      // timed out || feasibleCount.get<=0 || readyCount.get>0

      if (feasibleCount.get <= 0) // nothing can possibly happen now
        doOrElse()
      else if (readyCount.get > 0) // something is ready to happen
        findAndRun()
      else
        doAfter() // nothing is yet ready -- timed out

      log(TERM, s"alt terminated: $this)")
    }

  //    /** Run this alternation as a '''priserve''' */
  def priserve(): Unit = // withDebugger (registerWithDebugger)
    {
      altkind = "priserve"

      io.threadcso.repeat { // remaining time to wait before timeout
        val wait = if (afterEvent == null) 0L else afterEvent.deadline()

        registerReadiness()

        awaitSomething(wait)

        // timed out || feasibleCount.get==0 || readyCount.get>0

        if (feasibleCount.get <= 0) // nothing can possibly happen now
          doOrElseAndStop()
        else if (readyCount.get > 0) // something is ready to happen
          findAndRun()
        else
          doAfter() // nothing is yet ready -- timed out

        poll.reInitialize()
      }
      log(TERM, s"priserve terminated: $this")

    }

  /** Run this alternation as a '''serve''' */
  def serve(): Unit = // withDebugger (registerWithDebugger)
    {
      altkind = "serve"

      io.threadcso.repeat { // remaining time to wait before timeout
        val wait = if (afterEvent == null) 0L else afterEvent.deadline()

        registerReadiness()

        awaitSomething(wait)

        // timed out || feasibleCount.get<=0 || readyCount.get>0

        if (feasibleCount.get <= 0) // nothing can possibly happen now
          doOrElseAndStop()
        else if (readyCount.get > 0) // something is ready to happen
          findFairlyAndRun()
        else
          doAfter() // nothing is yet ready -- timed out

        poll.reInitialize()
      }
      log(TERM, s"serve terminated: $this")
    }

}
