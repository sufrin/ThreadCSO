package io.threadcso.debug

/** The trait of the two logs. */
trait LogT[A] {

  /** Add x to the log, by thread me, if (mask & bits) == bits. */
  def add(me: Int, x: A, bits: Int = 0): Unit

  /** Get the log events, in the order they were added. */
  def get: Array[A]

  /** Write the log to a file. */
  def toFile(fname: String = "/tmp/logFile") = {
    val writer = new java.io.PrintWriter(fname)
    val events = get
    for (e <- events) writer.write(s"$e\n")
    writer.flush(); writer.close()
  }

  def writeToFileOnShutdown(fname: String = "/tmp/logFile") = {
    val thread = new Thread(new Runnable { def run = { toFile(fname) } })
    java.lang.Runtime.getRuntime().addShutdownHook(thread)
  }
}

// =======================================================

import scala.collection.mutable.ArrayBuffer

/** A log object, to be used by p threads, to store log objects of type A.
  *
  * Internally, each thread uses a thread-local log, storing each logged item
  * together with a timestamp. Thus this class assumes that the operating system
  * provides timestamps that are consistent across cores. This assumption is
  * believed to be sound on Linux, but *not* on Windows. Windows users should
  * either install Linux, or use the less efficient SharedLog class.
  *
  * @param p
  *   the number of threads using the log.
  * @param mask
  *   a bitmask, indicating which logged events to store.
  */
class Log[A: scala.reflect.ClassTag](p: Int, mask: Int = 0xffffffff)
    extends LogT[A] {

  /** Thread t stores its date in theLogs(t). Each item is stored together with
    * a Long, giving the time elapsed since this Log object was created.
    */
  private val logs = Array.fill(p)(new ArrayBuffer[(Long, A)])

  /** The time this Log object was created. */
  private val startTime = System.nanoTime

  /** Add x to the log, by thread me, if (mask & bits) == bits. */
  def add(me: Int, x: A, bits: Int = 0) =
    if ((mask & bits) == bits) logs(me) += ((System.nanoTime - startTime, x))

  /** Have sentinels been added to the log yet? This is necessary to avoid
    * adding them again during the call to toFile.
    */
  private var sentinelsAdded = false

  /** Get the log events, in the order they were added. */
  def get: Array[A] = {
    // Add sentinels to each thread's log
    val Sentinel = Long.MaxValue
    // Clone logs in case another thread is working on them.
    val myLogs = Array.tabulate(p)(i => logs(i).clone)
    if (true || !sentinelsAdded) {
      for (log <- myLogs) log += ((Sentinel, null.asInstanceOf[A]))
      sentinelsAdded = true
    } else assert(myLogs.forall(_.last._1 == Sentinel))
    var size =
      myLogs.map(_.length).sum - p // total # elements, ignoring sentinels

    val result = new Array[A](size)
    val indices = Array.fill(p)(0)
    // Invariant: result[0..next) is the result of merging
    // { myLogs(i)[0..indices(i)) | i <- [0..p) }.

    for (next <- 0 until size) {
      // Find minimum of next values
      val minIx = (0 until p).minBy(i => myLogs(i)(indices(i))._1)
      val (ts, x) = myLogs(minIx)(indices(minIx))
      assert(ts < Sentinel)
      result(next) = x; indices(minIx) += 1
    }

    result
  }

}

// =======================================================

/** A log object.
  *
  * Internally, this log uses a shared buffer. Thus it is likely to perform
  * poorly. However, it avoids the reliance on the timestamping mechanism of the
  * Log class.
  *
  * @param mask
  *   a bitmask, indicating which logged events to store.
  */
class SharedLog[A: scala.reflect.ClassTag](mask: Int = 0xffffffff)
    extends LogT[A] {

  /** The buffer storing the logged items. */
  private val buffer = new ArrayBuffer[A]

  /** Add x to the log, by thread me, if (mask & bits) == bits. */
  def add(me: Int, x: A, bits: Int = 0) =
    if ((mask & bits) == bits) synchronized { buffer += x }

  /** Get the log events, in the order they were added.
    *
    * This call should not be concurrent to any add operation call.
    */
  def get: Array[A] = buffer.toArray
}
