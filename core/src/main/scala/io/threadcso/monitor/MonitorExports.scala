package io.threadcso.monitor

trait MonitorExports {
  type Monitor = io.threadcso.monitor.Monitor
  type FairMonitor = io.threadcso.monitor.FairMonitor
  type Condition = io.threadcso.monitor.LockSupportCondition

  /** A new `AbstractMonitor` object (either a `FairMonitor` or a `Monitor`)
    * whose lock-acquisition policy is as specified by `fair`.
    */
  def Monitor(
      fair: Boolean,
      name: String = null
  ): io.threadcso.monitor.AbstractMonitor =
    if (fair)
      new io.threadcso.monitor.FairMonitor(name)
    else
      new io.threadcso.monitor.Monitor(name)
}
