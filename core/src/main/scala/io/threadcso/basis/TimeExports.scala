package io.threadcso.basis

trait TimeExports {

  type Nanoseconds = io.threadcso.basis.Nanoseconds
  type Milliseconds = io.threadcso.basis.Milliseconds

  /** Number of nanoseconds in a nanosecond */
  val nanoSec: Nanoseconds = 1L

  /** Number of nanoseconds in a microsecond: `n*microSec` is n microseconds
    * expressed as nanoseconds
    */
  val microSec: Nanoseconds = 1000L * nanoSec

  /** Number of nanoseconds in a microsecond: `n*μS` is n microseconds expressed
    * as nanoseconds
    */
  val μS: Nanoseconds = microSec

  /** Number of nanoseconds in a millisecond: `n*milliSec` is n milliseconds
    * expressed as nanoseconds
    */
  val milliSec: Nanoseconds = 1000L * microSec

  /** Number of nanoseconds in a second: `n*Sec` is n seconds expressed as
    * nanoseconds
    */
  val Sec: Nanoseconds = 1000L * milliSec

  /** Number of nanoseconds in a minute: `n*Min` is n minutes expressed as
    * nanoseconds
    */
  val Min: Nanoseconds = 60L * Sec

  /** Number of nanoseconds in an hour: `n*Hour` is n hours expressed as
    * nanoseconds
    */
  val Hour: Nanoseconds = 60L * Min

  /** Number of nanoseconds in a day: `n*Day` is n days expressed as nanoseconds
    */
  val Day: Nanoseconds = 24L * Hour

  /** Convert a fractional time expressed in seconds to nanoseconds */
  def seconds(secs: Double): Nanoseconds =
    (secs * Sec).toLong

  /** Sleep for the given number of milliseconds. */
  @inline def sleepms(ms: Milliseconds): Unit = Thread.sleep(ms)

  /** Sleep for the given number of nanoseconds */
  @inline def sleep(ns: Nanoseconds): Unit =
    Thread.sleep(ns / milliSec, (ns % milliSec).toInt)

  /** Read the system nanosecond timer */
  @inline def nanoTime: Nanoseconds = System.nanoTime()

  /** Read the system millisecond timer */
  @inline def milliTime: Milliseconds = System.currentTimeMillis()

  /** This implicit class provides additional methods that support the legible
    * formatting of the `Nanoseconds` value `_elapsed` (which can be negative).
    */
  implicit class NanoTime(_elapsed: Nanoseconds) {
    private[this] val elapsed = if (_elapsed < 0) -_elapsed else _elapsed
    private[this] val sign = if (_elapsed < 0) "-" else ""
    private[this] val H = elapsed / Hour
    private[this] val M = (elapsed % Hour) / Min
    private[this] val S = (elapsed % Min) / Sec
    private[this] val mS = (elapsed % Sec) / milliSec
    private[this] val muS = (elapsed % milliSec) / microSec

    /** A string representing `_elapsed` in the form `H:M:S.ms,μs` */
    def HMS: String =
      f"$sign%s$H%d:$M%02d:$S%02d.$mS%03d,$muS%03d"

    /** A string representing `_elapsed` in the form `H:M:S.ms,μs` with hours,
      * minutes suppressed if zero.
      */
    def hms: String = {
      val h = if (H == 0) "" else f"$H%d:"
      val m = if (H == 0 && M == 0) "" else f"$M%02d:"
      f"$sign%s$h%s$m%s$S%02d.$mS%03d,$muS%03d"
    }

    /** `(elapsed-startTime).HMS` */
    def Δs: String = (elapsed - startTime).HMS

    /** `(elapsed-startTime).hms` */
    def δs: String = (elapsed - startTime).hms

    /** `(elapsed-startTime).HMS` */
    def Delta: String = (elapsed - startTime).HMS

    /** `(elapsed-startTime).hms` */
    def delta: String = (elapsed - startTime).hms
  }

  /** The value of `nanoTime` when the entire program was started -- useful only
    * for relative timing.
    */
  val startTime: Nanoseconds = nanoTime

}
