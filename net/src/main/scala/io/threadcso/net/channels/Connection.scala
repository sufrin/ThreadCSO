package io.threadcso.net.channels

import io.threadcso.alternation.channel.{InPort, OutPort}
import io.threadcso.process.Process
import io.threadcso.{Chan, OneOne, OneOneBuf, PROC}


/**
  * Abstraction of an already-established network connection
  */
trait Connection[-OUT, +IN] {
  val out: OutPort[OUT]
  val in:  InPort[IN]
}

/**
  * A process that when started (or forked) forwards material output to `out` to the network, and
  * forwards material arriving from the network to `in`, using the given `transfer` channels
  *
  */
class NetConnection[OUT, IN](val _name:           String = "<anonymous>",
                             val proxy:           NetProxy[OUT, IN],
                             val transferToNet:   Chan[OUT],
                             val transferFromNet: Chan[IN]) extends Connection[OUT, IN] with PROC {
  override def toString: String = s"NetConnection(${_name}, $proxy)"
  val out: OutPort[OUT] = transferToNet
  val in:  InPort[IN]   = transferFromNet

  private val copiers: PROC  = (proxy.CopyToNet(transferToNet) || proxy.CopyFromNet(transferFromNet)).withName(_name)

  def asUDP: TypedUDPChannel[OUT,IN] = proxy match {
    case channel: TypedUDPChannel[OUT,IN] => channel
  }

  def asSSL: TypedSSLChannel[OUT, IN] = proxy match {
    case channel: TypedSSLChannel[OUT, IN] => channel
  }

  def asTCP: TypedTCPChannel[OUT, IN] = proxy match {
    case channel: TypedTCPChannel[OUT, IN] => channel
  }

  /** Run the copiers in the current thread */
  def apply(): Unit = copiers()

  /**
    * Acquire a thread, and run the copiers in it
    */
  def fork: Process.Handle = copiers.fork
}

/**
  *  A factory for `NetConnection`s that deals with the details of
  *  allocating transfer channels, given the `NetProxy` that will be 
  *  using them.
  */
object NetConnection {
  def apply[OUT, IN](name: String, proxy: NetProxy[OUT, IN], transferToNet: Chan[OUT], transferFromNet: Chan[IN]): NetConnection[OUT,IN] =
    new NetConnection[OUT, IN](name, proxy, transferToNet, transferFromNet)

    /**
      *  Build a `NetConnection` from `proxy` with output/input buffers of the specified sizes. A size of `0`
      *  specifies unbuffered output/input
      */
    def apply[OUT, IN](proxy: NetProxy[OUT, IN], name: String=""): NetConnection[OUT, IN] = {
      val outName = s"NetConnection($name).out"
      val inName  = s"NetConnection($name).in"
      val outSize = ChannelOptions.outChanSize
      val inSize  = ChannelOptions.inChanSize
      val out: Chan[OUT] = if (outSize <= 0) OneOne[OUT](outName) else OneOneBuf[OUT](outSize, outName)
      val in: Chan[IN] = if (inSize <= 0) OneOne[IN](inName) else OneOneBuf[IN](inSize, inName)
      new NetConnection[OUT, IN](name, proxy, transferToNet = out, transferFromNet = in)
    }
}
