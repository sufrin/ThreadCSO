package io.threadcso.net

import io.threadcso.PROC
import io.threadcso.net.transport.{
  Options,
  NetConnection,
  TypedTransportFactory,
  TypedTCPTransport
}

import java.net.InetSocketAddress

/**
  * A factory for (client) `NetConnection`s that use TCP as their transport, and for servers
  * whose sessions use `NetConnection`s.
  */
object TCPConnection {
  val log = new ox.logging.Log("TCPConnection")

  /**
    * A connection using a synchronous network channel bound to the given address. This
    * is not the usual way to offer service at a given address (use a server).
    * @see NetConnection
    */
  def bound[OUT, IN](
                      address: InetSocketAddress,
                      factory: TypedTransportFactory[OUT, IN],
                      name: String = ""
  ): NetConnection[OUT, IN] = {
    val channel = TCPTransport.bound[OUT, IN](address, factory)
    transport.NetConnection(channel, name)
  }

  /**
    * A connection that is connected to the given address
    * @see NetConnection
    */
  def connected[OUT, IN](
                          address: InetSocketAddress,
                          factory: TypedTransportFactory[OUT, IN],
                          name: String = ""
  ): NetConnection[OUT, IN] = {
    val channel = TCPTransport.connected[OUT, IN](address, factory)
    val connected = transport.NetConnection(channel, name)
    if (log.logging) log.finer(s"TCPConnection.connected($connected)")
    connected
  }

  /**
    * Start a server offering service at the given port. In response to
    * a connection made (by `connected`) to the given port, apply the
    * given `session` with a suitable `NetConnection` as argument. The
    * session MUST run or fork its argument connection. The transfer buffer
    * sizes of each connection are specified by the values of
    * `Options.{inConSize, outConSize}` at the moment `server` is invoked.
    *
    * @see NetConnection
    */
  def server[OUT, IN](
                       port: Int,
                       backlog: Int,
                       factory: TypedTransportFactory[OUT, IN],
                       name: => String = ""
  )(session: NetConnection[OUT, IN] => Unit): PROC = {
    val ocs = Options.outChanSize
    val ics = Options.inChanSize
    TCPTransport.server(port, backlog, factory) {
      case tcpChannel: TypedTCPTransport[OUT, IN] =>
        val connection =
          Options.withOptions(outChanSize = ocs, inChanSize = ics) {
            transport.NetConnection[OUT, IN](tcpChannel, name)
          }
        if (log.logging)
          log.finer(
            s"TCPConnection.server($port, $backlog).session($connection)"
          )
        session(connection)
    }
  }

}
