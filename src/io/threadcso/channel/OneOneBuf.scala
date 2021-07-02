package io.threadcso.channel
import io.threadcso.basis.NameGenerator

/**
  * Static generator for OneOneBuf
  *
  * @see [[io.threadcso.channel.OneOneBuf]]
  */
object OneOneBuf extends NameGenerator("OneOneBuf") {
  def apply[T](size: Int,  name: String=newName()): OneOneBuf[T] =
    new OneOneBuf[T](size, name)
}

/**
  * A general-purpose shared buffered channel, bounded by `size` if it is positive,
  * and unbounded otherwise.
  *
  * The buffer can be shared by any number of readers and writers simultaneously, and (unlike [[OneOne]] channel)
  * there is no dynamic check for reader-overtaking (more than a single process trying to read simultaneously)
  * or writer overtaking.
  *
  * It closes completely after `closeIn` has been invoked,
  * or after  `closeOut` has been invoked and the buffer is empty.
  *
  * @param size    buffer is bounded by `size` if it is strictly positive, else unbounded
  * @param name    name (for the debugger)
  * @tparam T      type of value transmitted by the buffer
  */
class OneOneBuf[T](size: Int, name: String) extends N2NBuf[T](size, 1, 1, name)

