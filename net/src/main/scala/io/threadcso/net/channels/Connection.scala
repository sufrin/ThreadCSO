package io.threadcso.net.channels

import io.threadcso.alternation.channel.{InPort, OutPort}
import io.threadcso.process.Process
import io.threadcso.{Chan, OneOne, OneOneBuf, PROC}


/**
  * Abstraction of an already-established connection between peers. The connection
  * usually (but not necessarily) used network byte-streams.
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
  * A process that implements a `Connection[OUT,IN]` with a peer. When it is running it forwards
  * material output to `out` to the peer, and forwards material arriving from the peer  to `in`
  * using the given `transfer` channels.
  * @param _name the name of the connection
  * @param proxy the `NetProxy` view of the typed channel that transports the byte streams.
  * @param transferToNet the channel used to copy values to the network.
  * @param transferFromNet the channel used to copy values from the network.
  *
  *
  * @see NetConnection.apply
  */
class NetConnection[OUT, IN](val _name:           String = "<anonymous>",
                             val proxy:           NetProxy[OUT, IN],
                             val transferToNet:   Chan[OUT],
                             val transferFromNet: Chan[IN]) extends Connection[OUT, IN] with PROC {
  override def toString: String = s"NetConnection(${_name}, $proxy)"
  val out: OutPort[OUT] = transferToNet
  val in:  InPort[IN]   = transferFromNet

  private val copiers: PROC  = (proxy.CopyToNet(transferToNet) || proxy.CopyFromNet(transferFromNet)).withName(_name)

  /** The channel underlying the `proxy`: it must be a `TypedUDP` channel. */
  def asUDP: TypedUDPChannel[OUT,IN] = proxy match {
    case channel: TypedUDPChannel[OUT,IN] => channel
  }

  /** The channel underlying the `proxy`: it must be a `TypedSSL` channel. */
  def asSSL: TypedSSLChannel[OUT, IN] = proxy match {
    case channel: TypedSSLChannel[OUT, IN] => channel
  }

  /** The channel underlying the `proxy`: it must be a `TypedSSL` channel. */
  def asTCP: TypedTCPChannel[OUT, IN] = proxy match {
    case channel: TypedTCPChannel[OUT, IN] => channel
  }

  /**
    *  Run the copiers in the current thread.
    */
  def apply(): Unit = copiers()

  /**
    * Acquire a thread, and run the copiers in it
    */
  def fork: Process.Handle = copiers.fork

  /**
    * Acquire a thread, and run the proxy's copiers in it
    */
  def open(): Unit = { fork; () }

  /**
    * Close the input and output channels, thereby terminating the proxy's copiers.
    */
  def close(): Unit = {
    in.closeIn()
    out.closeOut()
  }
}

/**
  *  A factory for `NetConnection`s that can deal with the details of
  *  allocating transfer channels given the `NetProxy` that will be
  *  using them.
  */
object NetConnection {
  /** Return a new  `NetConnection` object as specified.  */
  def apply[OUT, IN](name: String, proxy: NetProxy[OUT, IN], transferToNet: Chan[OUT], transferFromNet: Chan[IN]): NetConnection[OUT,IN] =
    new NetConnection[OUT, IN](name, proxy, transferToNet, transferFromNet)

    /**
      *  Return a `NetConnection` from `proxy` after allocating transfer channel buffers of the sizes specified
      *  by `Options.outChanSize`, and `Options.inChanSize`. A size of `0` specifies a synchronous (unbuffered, `OneOne`)
      *  channel -- at the risk of deadlock.
      */
    def apply[OUT, IN](proxy: NetProxy[OUT, IN], name: String=""): NetConnection[OUT, IN] = {
      val outName = s"NetConnection($name).out"
      val inName  = s"NetConnection($name).in"
      val outSize = Options.outChanSize
      val inSize  = Options.inChanSize
      val out: Chan[OUT] = if (outSize <= 0) OneOne[OUT](outName) else OneOneBuf[OUT](outSize, outName)
      val in: Chan[IN] = if (inSize <= 0) OneOne[IN](inName) else OneOneBuf[IN](inSize, inName)
      new NetConnection[OUT, IN](name, proxy, transferToNet = out, transferFromNet = in)
    }
}
