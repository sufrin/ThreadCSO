package io.threadcso.monitor

/** A class that provides re-entrant first-come-first-served locking and
  * condition variables
  *
  * @see
  *   [[io.threadcso.monitor.AbstractMonitor]] for usage
  */

class FairMonitor(name: String = null) extends AbstractMonitor(true, name)

object FairMonitor {
  def apply(name: String = null): AbstractMonitor = new FairMonitor(name)
}
