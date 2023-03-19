package io.threadcso.alternation

trait ChannelExports {

  /** Type of an '''alt-capable''' channel as returned by the standard factory
    * methods of the `io.threadcso` API.
    */
  type Chan[T] = io.threadcso.alternation.channel.Chan[T]

  type ??[T] = io.threadcso.alternation.channel.InPort[T]
  type !![T] = io.threadcso.alternation.channel.OutPort[T]

  // type InPort[T] = io.threadcso.channel.InPort[T]
  // type OutPort[T] = io.threadcso.channel.OutPort[T]

  import channel.SharedAltCapableChannel

  /** Return a synchronous '''alt-capable''' channel (with the given `name`)
    * designed for point-to-point communication. There is a (weak) dynamic check
    * against multiple processes writing/reading simultaneously.
    */
  def OneOne[T](name: String = null): Chan[T] =
    new channel.OneOne[T](name)

  /** Return a synchronous '''alt-capable''' channel (with an
    * automatically-generated `name`) designed for point-to-point communication.
    * There is a (weak) dynamic check against multiple processes writing/reading
    * simultaneously.
    */
  def OneOne[T]: Chan[T] =
    new channel.OneOne[T](name = null)

  /** Return a synchronous '''alt-capable''' channel (with the given `name`)
    * designed for shared synchronous communication between `writers` writers,
    * and `readers` readers. When all writers have invoked the method `closeOut`
    * (or all readers the method `closeIn`), the channel closes. If either
    * `writers` or `readers` is nonpositive, then the channel can be closed an
    * unbounded number of times in the associated direction.
    */
  def N2N[T](
      writers: Int,
      readers: Int,
      name: String = null,
      fairOut: Boolean = false,
      fairIn: Boolean = false
  ): SharedAltCapableChannel[T] =
    new channel.N2N[T](
      writers,
      readers,
      name,
      fairOut,
      fairIn
    )

  /** Return a buffered '''alt-capable''' channel (with the given `name`)
    * designed for communication between a writer and a reader. This is
    * functionally equivalent to `N2NBuf(size, 1, 1, name)`.
    */
  def OneOneBuf[T](size: Int, name: String = null): Chan[T] =
    new channel.OneOneBuf[T](size, name)

  /** Return a buffered '''alt-capable''' channel (with the given `name`)
    * designed for shared communication between `writers` writers, and `readers`
    * readers. When all writers have invoked the method `closeOut` (or all
    * readers the method `closeIn`), the channel closes. If either `writers` or
    * `readers` is nonpositive, then the channel can be closed an unbounded
    * number of times in the associated direction.
    */
  def N2NBuf[T](
      size: Int,
      writers: Int,
      readers: Int,
      name: String = null,
      fairOut: Boolean = false,
      fairIn: Boolean = false
  ): SharedAltCapableChannel[T] =
    new channel.N2NBuf[T](
      size,
      writers,
      readers,
      name,
      fairOut,
      fairIn
    )

  /** Abbreviation for N2N(0, 1, name) */
  def ManyOne[T](name: String = null): SharedAltCapableChannel[T] =
    N2N(writers = 0, readers = 1, name)
  def ManyOne[T]: SharedAltCapableChannel[T] = ManyOne[T]()

  /** Abbreviation for N2N(1, 0, name) */
  def OneMany[T](name: String = null): SharedAltCapableChannel[T] =
    N2N(writers = 1, readers = 0, name)
  def OneMany[T]: SharedAltCapableChannel[T] = OneMany[T]()

  /** Abbreviation for N2N(0, 0, name) */
  def ManyMany[T](name: String = null): SharedAltCapableChannel[T] =
    N2N(writers = 0, readers = 0, name)
  def ManyMany[T]: SharedAltCapableChannel[T] = ManyMany[T]()

  /** This implicit class is used in the implementation of guarded I/O-event
    * notation. The operator `&&` replaced the operator `&&&` in CSO version
    * 1.2.
    */
  implicit class Guarded(guard: => Boolean) {
    def &&[T](port: channel.InPort[T]) =
      new channel.GuardedInPort[T](() => guard, port)
    def &&[T](port: channel.OutPort[T]) =
      new channel.GuardedOutPort[T](() => guard, port)
    def &&[T](chan: channel.Chan[T]) =
      new channel.GuardedChan[T](() => guard, chan)
  }
}
