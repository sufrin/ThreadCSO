package io.threadcso.alternation.channel

import io.threadcso.alternation.event.{OutPortEvent}

/** Guarded output port syntax */
class GuardedOutPort[T](guard: () => Boolean, port: OutPort[T]) {

  /** Construct a boolean-guarded output event for use in an alternation. */
  def =!=>(gen: => T) = new OutPortEvent(guard, port, () => gen)
}
