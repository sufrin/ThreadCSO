package ox.net

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
  * forwards material arriving from the network to `in`.
  *
  */
class NetConnection[OUT, IN](val _name: String = "", val proxy: NetProxy[OUT, IN], val toNet: Chan[OUT], val fromNet: Chan[IN]) extends Connection[OUT, IN] with PROC {
  val out: OutPort[OUT] = toNet
  val in:  InPort[IN]   = fromNet
  val daemon: PROC      = (proxy.CopyToNet(toNet) || proxy.CopyFromNet(fromNet)).withName(_name)

  /** Run the daemon in the current thread */
  def apply(): Unit = daemon()

  /**
    * Acquire a thread, and run the daemon in it
    */
  def fork: Process.Handle = daemon.fork
}

/**
  *  A factory for `NetConnection`s that deals with the details of
  *  allocating transfer channels.
  */
object NetConnection {
  def apply[OUT, IN](name: String, proxy: NetProxy[OUT, IN], toNet: Chan[OUT], fromNet: Chan[IN]): NetConnection[OUT,IN] =
    new NetConnection[OUT, IN](name, proxy, toNet, fromNet)

    /**
      *  Build a `NetConnection` from `proxy` with output/input buffers of the specified sizes. A size of `0`
      *  specifies unbuffered output/input
      */
    def apply[OUT, IN](proxy: NetProxy[OUT, IN], outSize: Int = 0, inSize: Int = 0, name: String=""): NetConnection[OUT, IN] = {
      val outName = s"NetConnection($name).out"
      val inName = s"NetConnection($name).in"
      val out: Chan[OUT] = if (outSize <= 0) OneOne[OUT](outName) else OneOneBuf[OUT](outSize, outName)
      val in: Chan[IN] = if (inSize <= 0) OneOne[IN](inName) else OneOneBuf[IN](inSize, inName)
      new NetConnection[OUT, IN](name, proxy, toNet = out, fromNet = in)
    }
}