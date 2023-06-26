package io.threadcso.net

import io.threadcso._
import io.threadcso.net.transport.SocketOptions.{IPv4, IPv6}
import io.threadcso.net.transport.{Options, TypedTransportFactory, TypedTCPTransport}

import java.net._
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import scala.annotation.nowarn

/**
  *  The factory for `TypedTCPTransport`s, as well as servers using `TCP` transport.
  */
object TCPTransport {
  val log = new ox.logging.Log()

  /**
    *   Construct a synchronous network channel bound to the given socket address.
    *   This is NOT in general the way to open a local TCP channel for business.
    */
  def bound[OUT, IN](address: InetSocketAddress, factory: TypedTransportFactory[OUT, IN]): TypedTCPTransport[OUT, IN] = {
    @nowarn val family = address.getAddress match {
      case _: Inet4Address => IPv4
      case _: Inet6Address => IPv6
    }
    val socket = SocketChannel.open(family)
    val channel = factory.newTransport(socket) // (SocketChannel.open)
    channel.property("family") = family
    socket.bind(address)
    println(s"bound $socket")
    channel
  }

  /**
    *  Construct a synchronous network channel connected to the given socket address
    */
  def connected[OUT, IN](address: InetSocketAddress, factory: TypedTransportFactory[OUT, IN]): TypedTCPTransport[OUT, IN] = {
    @nowarn val family = address.getAddress match {
      case _: Inet4Address => IPv4
      case _: Inet6Address => IPv6
    }
    val socket = SocketChannel.open(family)
    val channel = factory.newTransport(socket)
    socket.connect(address)
    channel
  }


  /**
    * Start serving on the given `port`, invoking `session` on a `factory`-constructed `TypedTCPCHannel` for each accepted
    * socket. The channel is constructed using the default `Options`. This is, in general, the way to open a local port for business.
    * Here's an (incomplete) example of a trivial server that starts a new "reflect" session for each of its connecting clients.
    * {{{
    *   val reflectServer: PROC = TCPTransport.server(port, backlog=0, factory=CRLFFactory) {
    *     case channel: TypedTCPTransport[String, String] =>
    *       val fromClient = OneOne[String](name = "fromClient")
    *       val toClient = OneOne[String](name = "toClient")
    *       val toNet = channel.transportToNet(toClient).fork
    *       val fromNet = channel.transportFromNet(fromClient).fork
    *       fork (proc ("reflect") { repeat { fromClient ? { text => toClient ! text } } })
    *   }
    *   fork(reflectServer)
    * }}}
    *
    * TODO: a shutdown method other than interrupt
    *
    * @see SSLTransport for the corresponding SSL/TLS method.
    */
  def server[OUT, IN](port: Int, backlog: Int, factory: TypedTransportFactory[OUT, IN])(session: TypedTCPTransport[OUT, IN] => Unit): PROC =
      server(port, backlog) {
         case client: SocketChannel => session(factory.newTransport(client))
      }

  /**
    * Start serving on the given `port`, invoking `session` at each accepted socket. Use
    * `Options.protocolFamily` as the protocol family.
    *
    * TODO: a shutdown method other than interrupt
    */
  def server(port: Int, backlog: Int)(session: SocketChannel => Unit): PROC = proc("server") {
    val address = new InetSocketAddress(port)
    val channel = ServerSocketChannel.open(Options.protocolFamily)
    channel.bind(address, backlog)
    while (true) {
      val client: SocketChannel = channel.accept()
      session(client)
    }
  }

  /**
    * Start serving on the `port` specified by `address`, invoking `session` at each accepted socket.
    *
    * TODO: a shutdown method other than interrupt
    */
  def server(address: InetSocketAddress, backlog: Int)(session: SocketChannel => Unit): PROC = proc("server") {
    val channel = ServerSocketChannel.open
    channel.bind(address, backlog)
    while (true) {
      val client: SocketChannel = channel.accept()
      session(client)
    }
  }
}





