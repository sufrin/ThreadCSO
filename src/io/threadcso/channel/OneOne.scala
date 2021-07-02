package io.threadcso.channel

import java.util.concurrent.atomic._
import java.util.concurrent.locks.LockSupport

import io.threadcso.basis.{Identity, NameGenerator, parkUntilElapsedOr}

import scala.annotation.elidable
import scala.annotation.elidable.FINEST

/** Static generator for OneOne channel.
  *
  * @see [[io.threadcso.channel.OneOne]]
  * @param _name the name of the channel, as reported by the debugger
  * @tparam T the type of message transmitted by the channel
  */
object OneOne extends NameGenerator("OneOne")
{
  def apply[T](name: String = newName()): OneOne[T] = new OneOne[T](name)
}

/**
  * A synchronized channel to be used by at most one reader and writer process ''simultaneously''. There are
  * dynamic checks for ''simultaneous'' multiple reads (writes), but they are  are ''incomplete''.
  * They cannot reliably detect that multiple readers (writers) are running concurrently
  * with a single writer (reader) that is fast enough to keep up with their read (write) rate.
  *
  * The channel closes when either ``closeOut()`` or ``closeIn()`` or ``close()`` is invoked.
  *
  * @param name_ the name of the channel, as reported by the debugger
  * @tparam T the type of message transmitted by the channel
  */

class OneOne[T](name_ : String) extends SyncChan[T]
{
  private[this] val reader, writer = new AtomicReference[Thread]
  private[this] val closed, full   = new AtomicBoolean(false)
  private[this] var buffer: T      = _

  /** Count of the number of finished reads (writes) */
  // @elidable (FINEST) 
  val reads, writes = new AtomicLong(0)
  /** Increment count of finished reads */
  @elidable (FINEST) def finishedRead = reads.incrementAndGet()
  /** Increment count of finished writes*/
  @elidable (FINEST) def finishedWrite = writes.incrementAndGet
  /** Intelligible information about finished reads and writes */
  @elidable (FINEST) def finishedRW = s"(READ ${reads.get}, WRITTEN ${writes.get})"

  /** READY if not closed and there's a writer waiting to sync (else CLOSED or UNKNOWN)
    * (used only by alternation implementations)
    * */
  def inPortState: PortState =
    if (closed.get) CLOSEDSTATE else if (full.get) READYSTATE else UNKNOWNSTATE

  /** READY if not closed and there's a reader waiting and last writer has already synced (else CLOSED or UNKNOWN)
    * (used only by alternation implementations)
    * */
  def outPortState: PortState =
    if (closed.get) CLOSEDSTATE
    else if (!full.get && reader.get != null) READYSTATE else UNKNOWNSTATE

  def nameGenerator: NameGenerator = OneOne

  setName(name_)
  this.register()

  /** Capture (an approximation to) the current state for debugger  components */
  def currentState: String = {
    val wr = reader.get
    val ww = writer.get
    val result =
    if (ww == null && wr == null) "idle"
    else
    {
      if (ww != null)
        if (full.get)
          s"!($buffer) from ${ww.identity}"
        else
          s"! from ${ww.identity }"
      else
          s"? from ${wr.identity}"
    }
    result + finishedRW
  }

  override def toString: String =
    s"""${this.name }: ${
      if (closed.get) "(CLOSED)"
      else ""
    } $currentState"""

  /** Show (an approximation to) the current state on `out` */
  override
  def showState(out: java.io.PrintWriter) =
  {
    out.print(s"CHANNEL ${this.name }: ${nameGenerator.kind } ")
    if (closed.get) out.print("(CLOSED) ")
    out.print(currentState)
  }


  def !(value: T) =
  { checkOpen
    val current = Thread.currentThread
    val lastWriter=writer.getAndSet(current)
    assert(lastWriter == null, s"!($value) in ${current.identity } overtaking !($buffer) [${lastWriter.identity}]")
    buffer = value
    full.set(true)
    inPortEvent(READYSTATE)           // Announce state change to any alternation
    LockSupport.unpark(reader.get)    // DELIVER BUFFER TO READER
    while (!closed.get && full.get)   // AWAIT SYNC FROM READER
    {
      LockSupport.park(this)
    }
    if (full.get) checkOpen           // *** (see close) []
    writer.set(null)                  // READY
    finishedWrite                     // elided in non-instrumented compilations
  }

  // def ?(): T =
  // { checkOpen
  //   val current = Thread.currentThread
  //   val lastReader = reader.getAndSet(current)
  //   assert(lastReader==null, s"?() overtaking [${lastReader.identity}] in ${current.identity }")
  //   outPortEvent(READYSTATE)                   // Announce state change to any alternation
  //   do{
  //     while (!closed.get && !full.get)           // AWAIT BUFFER FROM WRITER
  //     {
  //       LockSupport.park(this)
  //     }
  //     checkOpen                                  // ** (see close)
  //     val result = buffer                        // ## (cf. ?? at ##)
  //     buffer = null.asInstanceOf[T]              // For the garbage-collector
  //   } while(!full.compareAndSet(true,false))             // EMPTY BUFFER; READY
  //   // If CAS fails, retry

  //   reader.set(null)

  //   LockSupport.unpark(writer.get/*AndSet(null)*/) // SYNC WRITER
  //   // LockSupport.unpark(writer.getAndSet(null)) // SYNC WRITER ***incorrect version*** 
  //   finishedRead                               // elided in non-instrumented compilations
  //   result
  // }

  def ?(): T = 
  { checkOpen
    val current = Thread.currentThread
    val lastReader = reader.getAndSet(current)
    assert(lastReader==null, s"?() overtaking [${lastReader.identity}] in ${current.identity }")
    outPortEvent(READYSTATE)                   // Announce state change to any alternation
    while (!closed.get && !full.get)           // AWAIT BUFFER FROM WRITER
    {
      LockSupport.park(this)  
    }
    checkOpen                                  // ** (see close)
    val result = buffer                        // ## (cf. ?? at ##)
    buffer = null.asInstanceOf[T]              // For the garbage-collector
    reader.set(null)                          
    full.set(false)                            // EMPTY BUFFER; READY
    LockSupport.unpark(writer.get/*AndSet(null)*/) // SYNC WRITER
    // LockSupport.unpark(writer.getAndSet(null)) // SYNC WRITER ***incorrect version*** 
    finishedRead                               // elided in non-instrumented compilations
    result
  }

  def close(): Unit =
  { if (!closed.getAndSet(true))                  // closing is idempotent
    { outPortEvent(CLOSEDSTATE)                   // Announce state change
      inPortEvent(CLOSEDSTATE)                    // Announce state change
      LockSupport.unpark(reader.getAndSet(null))  // Force a waiting reader to continue **
      LockSupport.unpark(writer.getAndSet(null))  // Force a waiting writer to continue ***
      this.unregister()                           // Debugger no longer interested
    }

  }

  /** Extended rendezvous read & compute, then sync */
  def ??[U](f: T => U): U =
  { checkOpen
    val current = Thread.currentThread
    val lastReader = reader.getAndSet(current)
    assert(lastReader==null, s"??(...) overtaking [${lastReader.identity}] in ${current.identity }")
    outPortEvent(READYSTATE)
    while (!closed.get && !full.get)
    {
      LockSupport.park(this)
    }
    checkOpen                                     // ** (see close)
    val result = f(buffer)                        // ## compute before the write sync: (cf. ? at ##)
    buffer = null.asInstanceOf[T]                 // For the garbage collector
    reader.set(null)                              
    full.set(false)                               // EMPTY BUFFER; READY
    LockSupport.unpark(writer.get/*AndSet(null)*/)    // SYNC WRITER
    finishedRead
    result
  }

  /** Ordinary rendezvous read & sync then compute */
  def ?[U](f: T => U): U = f(?())                 // Compute after the write sync

  def canInput: Boolean = !closed.get

  def closeIn(): Unit = close()

  def canOutput: Boolean = !closed.get

  def closeOut(): Unit = close()

  @inline private[this] def checkOpen =
      if (closed.get) {
         writer.set(null)
         reader.set(null)
        throw new Closed(name)
      }


  def readBefore(timeoutNS: Long): Option[T] =
  {
    assert(reader.get == null, s"?() overtaking [${reader.get.identity}] in ${Thread.currentThread.identity }")
    checkOpen
    val current = Thread.currentThread
    reader.set(current)
    outPortEvent(READYSTATE)
    val success = 0 < parkUntilElapsedOr(this, timeoutNS, closed.get || full.get)
    checkOpen
    val result = buffer
    buffer = null.asInstanceOf[T]
    reader.set(null)
    full.set(false)
    LockSupport.unpark(writer.get/*AndSet(null)*/)
    finishedRead
    if (success) Some(result) else None
  }

  def writeBefore(timeoutNS: Long)(value: T): Boolean =
  {
    assert(writer.get == null, s"!($value) in ${Thread.currentThread.identity } overtaking !($buffer) [${writer.get.identity}]")
    checkOpen
    buffer = value
    val current = Thread.currentThread
    writer.set(current)
    full.set(true)
    inPortEvent(READYSTATE) 
    LockSupport.unpark(reader.getAndSet(null))
    var success = 0 < parkUntilElapsedOr(this, timeoutNS, closed.get || !full.get)
    // if(!success) success = ! full.compareAndSet(true, false)
    if (!success) full.set(false)
    writer.set(null)
    checkOpen
    finishedWrite
    success
  }

}

/* 

1.2R5 -> 1.2R6: very-low-incidence deadlock between writes and reads
observed and diagnosed using csp/FDR by Gavin Lowe

Gavin's suggested solution (writer.getAndSet(null) -> writer.get)
to both read routines seems to solve the problem.

*/






