package io.threadcso.transform
import io.threadcso
import io.threadcso.channel.PortState
import io.threadcso.{!!, ??, Milliseconds, alternation}

/*
 *  Tools to transform the traffic through ports, by pre-output encoding
 *  or post-input decoding.
 */
object port {

  /** Deliver a port that `encode`s before outputting to `out` */
  def map[OUT, CODEDOUT](out: !![CODEDOUT])(encode: OUT => CODEDOUT): !![OUT] = new!![OUT] {
    def !(value: OUT): Unit = out ! (encode(value))
    def writeBefore(nsWait: Milliseconds)(value: OUT): Boolean = out.writeBefore(nsWait)(encode(value))
    def closeOut(): Unit = out.closeOut()
    def canOutput: Boolean = out.canOutput
    def name: String = out.name
    override def toString: String = name
    def outPortState: PortState = out.outPortState
    def registerOut(alt: alternation.Runnable, theIndex: Int): PortState = out.registerOut(alt, theIndex)
    def unregisterOut(): PortState = out.unregisterOut()
  }

  /** Deliver a port that `decode`s after inputting from `in` */
  def map[CODEDIN, IN](in: ??[CODEDIN])(decode: CODEDIN => IN): ??[IN] = new??[IN] {
    def ?(): IN = decode(in ? ())
    def ?[U](g: IN => U): U = g(decode(in ? ()))
    def readBefore(ns: Milliseconds): Option[IN] = in.readBefore(ns).map(decode)
    def ??[U](g: IN => U): U = in ?? { t => g(decode(t)) }
    def closeIn(): Unit = in.closeIn()
    def canInput: Boolean = in.canInput
    def name: String = in.name
    override def toString: String = in.name
    def inPortState: PortState = in.inPortState
    def registerIn(alt: alternation.Runnable, theIndex: Int): PortState = in.registerIn(alt, theIndex)
    def unregisterIn(): PortState = in.unregisterIn()
  }

  /** A non-alting `OutPort[T]` */
  type `!!-`[T] = threadcso.channel.OutPort[T]

  /** Deliver a port that `encode`s before outputting to `out` */
  def map[OUT, CODEDOUT](out: `!!-`[CODEDOUT])(encode: OUT => CODEDOUT): `!!-`[OUT] = new`!!-`[OUT] {
    def !(value: OUT): Unit = out.!(encode(value))
    def writeBefore(nsWait: Milliseconds)(value: OUT): Boolean = out.writeBefore(nsWait)(encode(value))
    def closeOut(): Unit = out.closeOut()
    def canOutput: Boolean = out.canOutput
    def name: String = out.name
    override def toString: String = out.name
    def outPortState: PortState = out.outPortState
  }


  /** A non-alting `InPort[CODEDIN]` */
  type `??-`[T] = threadcso.channel.InPort[T]

  /** Deliver a port that `decode`s after inputting from `in` */
  def map[CODEDIN, IN](in: `??-`[CODEDIN])(decode: CODEDIN => IN): `??-`[IN] = new`??-`[IN] {
    def ?(): IN = decode(in ? ())
    def ?[U](g: IN => U): U = g(decode(in ? ()))
    def readBefore(ns: Milliseconds): Option[IN] = in.readBefore(ns).map(decode)
    def ??[U](g: IN => U): U = in ?? { t => g(decode(t)) }
    def closeIn(): Unit = in.closeIn()
    def canInput: Boolean = in.canInput
    def name: String = in.name
    override def toString: String = in.name
    def inPortState: PortState = in.inPortState
  }
}
