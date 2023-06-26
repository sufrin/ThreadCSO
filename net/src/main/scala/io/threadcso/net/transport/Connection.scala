package io.threadcso.net.transport

import io.threadcso.alternation.channel.{InPort, OutPort}
import io.threadcso.process.Process
import io.threadcso.{Chan, OneOne, OneOneBuf, PROC}


/**
  * Abstraction of typed value transport between peers. The connection
  * usually (but not necessarily) uses network byte-streams.
  */
trait Connection[-OUT, +IN] {
  /** Values output to `out` are transmitted to the peer. */
  val out: OutPort[OUT]
  /** Values transmitted from the peer can be input from `in`  */
  val in:  InPort[IN]
  /** Do anything that's necessary to start the connection functioning */
  def open(): Unit
  /**
    * Do anything that's necessary to stop the connection functioning and
    * release any resources the connection uses.
    */
  def close(): Unit
}

/**
  * A process that implements a `Connection[OUT,IN]` with a peer using network transport. When running it forwards
  * material output to `out` to the peer, and forwards material arriving from the peer  to `in`
  * using the given `transfer`  `Chan`nels.
  * @param _name the name of the connection
  * @param transport the `TypedTransport` view of the typed channel that transports the byte streams.
  * @param transferToNet the channel used to transfer values to the network.
  * @param transferFromNet the channel used to transfer values from the network.
  *
  *
  * @see NetConnection.apply
  */
class NetConnection[OUT, IN](val _name:           String = "<anonymous>",
                             val transport:       TypedTransport[OUT, IN],
                             val transferToNet:   Chan[OUT],
                             val transferFromNet: Chan[IN]) extends Connection[OUT, IN] with PROC {
  override def toString: String = s"NetConnection(${_name}, $transport)"
  val out: OutPort[OUT] = transferToNet
  val in:  InPort[IN]   = transferFromNet

  private val daemons: PROC  = (transport.transportToNet(transferToNet) || transport.transportFromNet(transferFromNet)).withName(_name)

  /** The underlying `transport`: viewed as a `TypedUDPTransport` if appropriate. */
  def asUDP: TypedUDPTransport[OUT,IN] = transport match {
    case channel: TypedUDPTransport[OUT,IN] => channel
  }

  /** The underlying `transport`: viewed as a `TypedSSLTransport` if appropriate. */
  def asSSL: TypedSSLTransport[OUT, IN] = transport match {
    case channel: TypedSSLTransport[OUT, IN] => channel
  }

  /** The underlying `transport`: viewed as a `TypedTCPTransport` if appropriate. */
  def asTCP: TypedTCPTransport[OUT, IN] = transport match {
    case channel: TypedTCPTransport[OUT, IN] => channel
  }

  /**
    *  Run the transport daemons in the current thread.
    */
  def apply(): Unit = daemons()

  /**
    * Acquire a thread, and run the transport daemons in it
    */
  def fork: Process.Handle = daemons.fork

  /**
    * Acquire a thread, and run the transport's copiers in it
    */
  def open(): Unit = { fork; () }

  /**
    * Close the input and output transport, thereby terminating the transport's copiers.
    */
  def close(): Unit = {
    in.closeIn()
    out.closeOut()
  }
}

/**
  *  A factory for `NetConnection`s that can deal with the details of
  *  allocating transfer transport given the `TypedTransport` that will be
  *  using them.
  */
object NetConnection {
  /** Return a new  `NetConnection` object as specified.  */
  def apply[OUT, IN](name: String, transport: TypedTransport[OUT, IN], transferToNet: Chan[OUT], transferFromNet: Chan[IN]): NetConnection[OUT,IN] =
    new NetConnection[OUT, IN](name, transport, transferToNet, transferFromNet)

    /**
      *  Return a `NetConnection` from `transport` after allocating transfer channel buffers of the sizes specified
      *  by `Options.outChanSize`, and `Options.inChanSize`. A size of `0` specifies a synchronous (unbuffered, `OneOne`)
      *  channel -- at the risk of deadlock.
      */
    def apply[OUT, IN](transport: TypedTransport[OUT, IN], name: String=""): NetConnection[OUT, IN] = {
      val outName = s"NetConnection($name).out"
      val inName  = s"NetConnection($name).in"
      val outSize = Options.outChanSize
      val inSize  = Options.inChanSize
      val out: Chan[OUT] = if (outSize <= 0) OneOne[OUT](outName) else OneOneBuf[OUT](outSize, outName)
      val in: Chan[IN] = if (inSize <= 0) OneOne[IN](inName) else OneOneBuf[IN](inSize, inName)
      new NetConnection[OUT, IN](name, transport, transferToNet = out, transferFromNet = in)
    }
}
