package io.threadcso.net

import io.threadcso._
import io.threadcso.net.channels.SocketOptions.{IPv4, IPv6}
import io.threadcso.net.channels.{ChannelOptions, TypedChannelFactory, TypedTCPChannel}

import java.net._
import java.nio.channels.{ServerSocketChannel, SocketChannel}


object TCPChannel {
  val log = new ox.logging.Log()

  /**
    *   Construct a synchronous network channel bound to the given socket address.
    *   This is NOT in general the way to open a local TCP channel for business.
    */
  def bound[OUT, IN](address: InetSocketAddress, factory: TypedChannelFactory[OUT, IN]): TypedTCPChannel[OUT, IN] = {
    val family = address.getAddress match {
      case _: Inet4Address => IPv4
      case _: Inet6Address => IPv6
    }
    val socket = SocketChannel.open(family)
    val channel = factory.newChannel(socket) // (SocketChannel.open)
    channel.property("family") = family
    socket.bind(address)
    println(s"bound $socket")
    channel
  }

  /**
    *  Construct a synchronous network channel connected to the given socket address
    */
  def connected[OUT, IN](address: InetSocketAddress, factory: TypedChannelFactory[OUT, IN]): TypedTCPChannel[OUT, IN] = {
    val socket = SocketChannel.open
    val channel = factory.newChannel(socket)
    println(s"connected pre connect: $socket")
    socket.connect(address)
    println(s"connected post connect: $socket")
    channel
  }


  /**
    * Start serving on the given `port`, invoking `session` on a `factory`-constructed `TypedTCPCHannel` for each accepted
    * socket. The channel is constructed using the default `ChannelOptions`. This is, in general, the way to open a local port for business.
    * Here's an (incomplete) example of a trivial server that starts a new "reflect" session for each of its connecting clients.
    * {{{
    *   val reflectServer: PROC = TCPChannel.server(port, backlog=0, factory=CRLFFactory) {
    *     case channel: TypedTCPChannel[String, String] =>
    *       val fromClient = OneOne[String](name = "fromClient")
    *       val toClient = OneOne[String](name = "toClient")
    *       val toNet = channel.CopyToNet(toClient).fork
    *       val fromNet = channel.CopyFromNet(fromClient).fork
    *       fork (proc ("reflect") { repeat { fromClient ? { text => toClient ! text } } })
    *   }
    *   fork(reflectServer)
    * }}}
    *
    * TODO: a shutdown method other than interrupt
    *
    * @see SSLChannel for the corresponding SSL/TLS method.
    */
  def server[OUT, IN](port: Int, backlog: Int, factory: TypedChannelFactory[OUT, IN]) (session: TypedTCPChannel[OUT, IN] => Unit): PROC =
      server(port, backlog) {
         case client: SocketChannel => session(factory.newChannel(client))
      }

  /**
    * Start serving on the given `port`, invoking `session` at each accepted socket.
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
}





