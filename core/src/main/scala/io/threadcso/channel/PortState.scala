package io.threadcso.channel

/** A type denoting the state of readiness/commitment of a port. Used in the
  * implementation of alternations.
  */
sealed trait PortState {

  /** Abbreviation for use by the debugger */
  def toStateString: String
}

/** The port is closed */
case object CLOSEDSTATE extends PortState {
  override def toString = "CLS"
  def toStateString = "CLS: "
}

/** The readiness of the port is unknown */
case object UNKNOWNSTATE extends PortState {
  override def toString = "UNK"
  override def toStateString = ""
}

/** The port's channel is in a state that guarantees the next unconditional
  * input (on an `InPort`) or output (on an `OutPort`) action on the port can be
  * invoked with no danger of an unbounded wait because: <ul> <li> In the case
  * of a synchronized channel, a peer has (already) committed to the
  * complementary output/input on the dual port, or </li>
  *
  * <li> In the case of a buffer, the buffer is in an appropriate state, viz:
  * nonempty for an `InPort` to be ready, and nonfull for an `OutPort` to be
  * ready.</li> </ul>
  */
case object READYSTATE extends PortState {
  override def toString = "RDY"
  override def toStateString = "RDY: "
}
