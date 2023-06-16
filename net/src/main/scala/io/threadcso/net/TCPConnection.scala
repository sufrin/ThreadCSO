package io.threadcso.net

import io.threadcso.PROC
import io.threadcso.net.channels.{
  ChannelOptions,
  NetConnection,
  TypedChannelFactory,
  TypedTCPChannel
}

import java.net.InetSocketAddress

/** A factory for connections using TCP as transport. */
object TCPConnection {
  val log = new ox.logging.Log("TCPConnection")

  /** A connection using a synchronous network channel bound to the given address. This
    * is not the usual way to offer service at a given address (use a server).
    */
  def bound[OUT, IN](
      address: InetSocketAddress,
      factory: TypedChannelFactory[OUT, IN],
      name: String = ""
  ): NetConnection[OUT, IN] = {
    val channel = TCPChannel.bound[OUT, IN](address, factory)
    channels.NetConnection(channel, name)
  }

  /** A connection that is connected to the given address */
  def connected[OUT, IN](
      address: InetSocketAddress,
      factory: TypedChannelFactory[OUT, IN],
      name: String = ""
  ): NetConnection[OUT, IN] = {
    val channel = TCPChannel.connected[OUT, IN](address, factory)
    val connected = channels.NetConnection(channel, name)
    if (log.logging) log.finer(s"TCPConnection.connected($connected)")
    connected
  }

  /** Start a server offering service at the given port. In response to
    * a connection made (by `connected`) to the given port, apply the
    * given `session` with a suitable `NetConnection` as argument. The
    * session MUST run or fork its argument connection. The transfer buffer
    * sizes of each connection are specified by the values of
    * `ChannelOptions.{inConSize, outConSize}` at the moment `server` is called.
    */
  def server[OUT, IN](
      port: Int,
      backlog: Int,
      factory: TypedChannelFactory[OUT, IN],
      name: => String = ""
  )(session: NetConnection[OUT, IN] => Unit): PROC = {
    val ocs = ChannelOptions.outChanSize
    val ics = ChannelOptions.inChanSize
    TCPChannel.server(port, backlog, factory) {
      case tcpChannel: TypedTCPChannel[OUT, IN] =>
        val connection =
          ChannelOptions.withOptions(outChanSize = ocs, inChanSize = ics) {
            channels.NetConnection[OUT, IN](tcpChannel, name)
          }
        if (log.logging)
          log.finer(
            s"TCPConnection.server($port, $backlog).session($connection)"
          )
        session(connection)
    }
  }

}
