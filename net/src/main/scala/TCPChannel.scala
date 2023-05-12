package ox.net

import io.threadcso._
import ox.net.SocketOptions.IPv4

import java.io._
import java.net.{InetSocketAddress, ProtocolFamily, SocketAddress}
import java.nio.channels.{ServerSocketChannel, SocketChannel}


object TCPChannel {

  def bound[OUT, IN](address: SocketAddress, factory: TypedChannelFactory[OUT, IN], family: ProtocolFamily = IPv4): TypedTCPChannel[OUT, IN] = {
    val socket = SocketChannel.open(family)
    val channel = factory.newChannel(SocketChannel.open)
    channel.property("family") = family
    socket.bind(address)
    channel
  }

  /** Construct a synchronous network channel connected to the given socket address */
  def connected[OUT, IN](address: SocketAddress, factory: TypedChannelFactory[OUT, IN]): TypedTCPChannel[OUT, IN] = {
    val socket = SocketChannel.open
    val channel = factory.newChannel(socket)
    socket.connect(address)
    channel
  }

  /**
    * Start serving on the given `port`, invoking `session` at each accepted socket.
    *
    * TODO: a shutdown method other than interrupt
    */
  def server(port: Int, backlog: Int)(session: SocketChannel => Unit): PROC = proc("server") {
    val address = new InetSocketAddress(port)
    val channel = ServerSocketChannel.open(ChannelOptions.protocolFamily)
    channel.bind(address, backlog)
    while (true) {
      val client: SocketChannel = channel.accept()
      session(client)
    }
  }

  /**
    * Start serving on the given `port`, invoking `session` on a `factory`-constructed `TypedTCPCHannel` for each accepted
    * socket. The channel is constructed using the default `ChannelOptions`.
    *
    * TODO: a shutdown method other than interrupt
    */
  def server[OUT, IN](port: Int, backlog: Int, factory: TypedChannelFactory[OUT, IN]) (session: TypedTCPChannel[OUT, IN] => Unit): PROC =
      server(port, backlog) {
         case client: SocketChannel => session(factory.newChannel(client))
      }
}



