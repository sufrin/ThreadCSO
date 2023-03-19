package io.threadcso.debug
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}

import io.SourceLocation.SourceLocation
import io.threadcso.basis.{Identity, Nanoseconds}
import io.threadcso.{NanoTime, nanoTime}

// import scala.annotation.elidable
// import scala.annotation.elidable.FINEST

/** A component to support the recording of logging information from CSO
  * programs. See [[io.threadcso.debug.Logging]] for an example of how an
  * elidable log can be specialized; and [[io.threadcso.alternation.Run]] for a
  * typical use.
  *
  * @param name
  *   name of the log
  * @param logSize
  *   the most recent `logSize` (or fewer) entries are recorded in a log.
  * @param mask
  *   (variable) bitmask for the log: a call to `log(bits,...)` makes an entry
  *   in the log iff `mask&bits==bits`
  *
  * {{{
  * @author Bernard Sufrin, Oxford
  * \$Revision: 238 $
  * \$Date: 2017-10-07 21:21:09 +0100 (Sat, 07 Oct 2017) $
  * }}}
  */
class Logger(val name: String, val logSize: Int, mask: Int = 0xffffffff)
    extends io.threadcso.debug.REGISTRY.Debuggable {
  override def toString = s"Logger($name, detail=$mask, size=$logSize)"

  this.register()

  private[this] val entries = new scala.collection.mutable.Queue[
    (Nanoseconds, String, SourceLocation, String)
  ]()

  /** Approximation to the current sequence of entries recorded by this logger.
    * Records consist of a timestamp (nanoseconds), a thread identifier, the
    * source location (of the log call), the text being logged.
    */
  val events: collection.Seq[(Nanoseconds, String, SourceLocation, String)] =
    entries

  /** Atomically add the timestamped text to the end of the log if
    * `mask&bits==bits`. Calls to this method are elided if the CSO library was
    * compiled with "-Xelide-below 350" (production versions are usually
    * so-compiled). We rely on built-in JVM synchronization for atomicity.
    */
  // @elidable(FINEST)
  def apply(bits: Int, text: => String)(implicit loc: SourceLocation): Unit = {
    if ((mask & bits) == bits) synchronized {
      val message = (nanoTime, Thread.currentThread.identity, loc, text)
      entries.enqueue(message)
      if (entries.length > logSize) entries.dequeue()
      ()
    }
  }

  /** Atomically add the timestamped text to the end of the log if
    * `mask&bits==bits. Calls to this method are not elidable. We rely on
    * built-in JVM synchronization for atomicity.
    */
  def log(bits: Int, text: => String)(implicit loc: SourceLocation): Unit = {
    if ((mask & bits) == bits) synchronized {
      val message = (nanoTime, Thread.currentThread.identity, loc, text)
      entries.enqueue(message)
      if (entries.length > logSize) entries.dequeue()
      ()
    }
  }

  /** Output the log to the given `PrintWriter` */
  override def showState(out: java.io.PrintWriter): Unit = {
    out.println(s"$toString Log")
    for ((time, threadId, source, event) <- entries)
      out.println(s"${time.delta}:: $threadId@$source: $event")
  }

  def printState(): Unit = {
    println(s"$toString Log")
    for ((time, threadId, source, event) <- entries)
      println(s"${time.delta}:: $threadId@$source: $event")
  }

}

/** Just `import io.threadcso.debug.Logger` if you wish to construct logs and
  * make log entries.
  */
object Logger {
  import io.SourceLocation._

  import scala.language.experimental.macros
  def sourceLoc: SourceLocation = macro sourceLocationMACRO
  def apply(name: String, logSize: Int, mask: Int = -1): Logger =
    new Logger(name, logSize, mask)
}
