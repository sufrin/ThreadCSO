package io.threadcso.monitor

/** A class that provides re-entrant locking and condition variables
  *
  * @see
  *   [[io.threadcso.monitor.AbstractMonitor]] for usage
  */
class Monitor(name: String = null) extends AbstractMonitor(false, name)

object Monitor {
  def apply(name: String = null): AbstractMonitor = new Monitor(name)
}
