package io.threadcso.channel

import io.threadcso.process.Stopped

/** Thrown by an attempt to read from or write to a port that is already closed
  * (or that closes before the termination of the read/write).
  */
class Closed(name: String) extends Stopped() {
  override def toString: String = "Closed(%s)".format(name)
}
