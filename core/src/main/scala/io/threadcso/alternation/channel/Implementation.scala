/** Each constructor in this package delivers a channel that augments the
  * functionality of the identically-named non-`alting` channel (buffer) with
  * the capacity to participate in `alt` and `serve` commands (and the
  * corresponding prioritised commands)
  */
package io.threadcso.alternation.channel

import io.threadcso.basis.NameGenerator
import io.threadcso.channel.PortState

/** A version of its namesake from [[io.threadcso.channel]] with the capacity to
  * participate in `alt` and `serve` commands
  *
  * @see
  *   [[io.threadcso.channel.OneOne]]
  */
class OneOne[T](name: String = null)
    extends io.threadcso.channel.OneOne[T](name)
    with AltCapableChannel[T] {
  override def inPortEvent(portState: PortState): Unit = setInPortStateImp(
    portState
  )
  override def outPortEvent(portState: PortState): Unit = setOutPortStateImp(
    portState
  )
}

/** Static generator for OneOne
  *
  * @see
  *   [[io.threadcso.alternation.channel.OneOne]]
  */
object OneOne extends NameGenerator("OneOne") {
  def apply[T](name: String = newName()): OneOne[T] = new OneOne[T](name)
}

/** Static generator for N2N
  *
  * @see
  *   [[io.threadcso.alternation.channel.N2N]]
  */
object N2N extends NameGenerator("N2N") {
  def apply[T](
      writers: Int,
      readers: Int,
      name: String = newName(),
      fairOut: Boolean = false,
      fairIn: Boolean = false
  ): N2NBuf[T] =
    N2N(writers, readers, name, fairOut, fairIn)
}

/** A version of its namesake from [[io.threadcso.channel]] with the capacity to
  * participate in `alt` and `serve` commands
  *
  * @see
  *   [[io.threadcso.channel.N2N]]
  */
class N2N[T](
    writers: Int,
    readers: Int,
    name: String = null,
    fairOut: Boolean = false,
    fairIn: Boolean = false
) extends io.threadcso.channel.N2N[T](writers, readers, name, fairOut, fairIn)
    with SharedAltCapableChannel[T] {
  override def inPortEvent(portState: PortState): Unit = setInPortStateImp(
    portState
  )
  override def outPortEvent(portState: PortState): Unit = setOutPortStateImp(
    portState
  )
}

/** Static generator for OneOneBuf
  *
  * @see
  *   [[io.threadcso.alternation.channel.OneOneBuf]]
  */
object OneOneBuf extends NameGenerator("OneOneBuf") {
  def apply[T](size: Int, name: String = newName()): OneOneBuf[T] =
    new OneOneBuf[T](size, name)
}

/** A version of its namesake from [[io.threadcso.channel]] with the capacity to
  * participate in `alt` and `serve` commands
  *
  * @see
  *   [[io.threadcso.channel.N2NBuf]]
  */
class OneOneBuf[T](size: Int, name: String)
    extends io.threadcso.channel.N2NBuf[T](
      size,
      writers = 1,
      readers = 1,
      name
    )
    with AltCapableChannel[T] {

  override def inPortEvent(portState: PortState): Unit =
    setInPortStateImp(portState)

  override def outPortEvent(portState: PortState): Unit =
    setOutPortStateImp(portState)

  override def nameGenerator: NameGenerator = OneOneBuf
}

/** Static generator for N2NBuf
  *
  * @see
  *   [[io.threadcso.alternation.channel.N2NBuf]]
  */
object N2NBuf extends NameGenerator("N2NBuf") {
  def apply[T](
      size: Int,
      writers: Int,
      readers: Int,
      name: String = newName(),
      fairOut: Boolean = false,
      fairIn: Boolean = false
  ): N2NBuf[T] =
    new N2NBuf(size, writers, readers, name, fairOut, fairIn)
}

/** A version of its namesake from [[io.threadcso.channel]] with the capacity to
  * participate in `alt` and `serve` commands
  *
  * @see
  *   [[io.threadcso.channel.N2NBuf]]
  */
class N2NBuf[T](
    size: Int,
    writers: Int,
    readers: Int,
    name: String,
    fairOut: Boolean = false,
    fairIn: Boolean = false
) extends io.threadcso.channel.N2NBuf[T](size, writers, readers, name)
    with SharedAltCapableChannel[T] {
  override def inPortEvent(portState: PortState): Unit = setInPortStateImp(
    portState
  )
  override def outPortEvent(portState: PortState): Unit = setOutPortStateImp(
    portState
  )
  override def nameGenerator: NameGenerator = N2NBuf
}
