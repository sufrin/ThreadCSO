package ox.net

import io.SourceLocation._
import ox.net.SocketOptions._
import ox.net.UDPChannel.UDP
import ox.net.codec.{Codec, EndOfInputStream}

import java.io.{EOFException, UTFDataFormatException}
import java.net._
import java.nio.channels._

object UDPChannel extends
{ val log = ox.logging.Logging.Log("UDPChannel")

  sealed trait UDP[T] {
    val address: SocketAddress
  }

  /**
    * Evidence of the arrival of an undecodeable packet that originated at the given address`. The
    * packet may well have been "silently truncated". There is no provision for
    * trying again to decode the undecodeable payload, but a higher-level protocol may
    * decide to send a "resend" notification.
    */
  case class Malformed[T](address: SocketAddress) extends UDP[T]

  /**
    * A `Datagram` originating at the given address, or to be sent to the
    * given address.
    *
    * In the latter case, if the address is `null` then the datagram is sent
    * to the currently-connected remote address.
    *
    *
    * @see UDPChannel.encode
    */
  case class Datagram[T](value: T, address: SocketAddress = null) extends UDP[T] {
    override def toString: String =
      value match {
        case s: String => if (s.length > 20) s"${s.substring(0, 20)}...#${s.length}" else s
        case _ => value.toString
      }

  }

  /**
    * @return a `UDPChannel[OUT,IN]` that sends outputs of the form `Datagram(OUT, destinationAddress)` from a
    *         local datagram socket at the given `address`, and receives inputs of form `Datagram(OUT, sourceAddress)`
    *         at that local datagram socket. The returned channel is open to receiving datagrams from any source; but may
    *         later be specialized to communicate with a particular address by `connect(particularAddress)`
    * @param address the socket address to which the channel will be bound.
    * @param factory builds the typed channel from the (untyped) datagram socket
    * @param family the protocol family
    * @tparam OUT type of data encoded and sent out in datagram packets
    * @tparam IN type of data read in datagram packets then decoded
    *
    */
  def bind[OUT,IN](address: InetSocketAddress, factory: TypedChannelFactory[OUT, IN], family: ProtocolFamily = IPv4): UDPChannel[OUT,IN] =
  { val socket = DatagramChannel.open(family)
    val channel = new UDPChannel[OUT,IN](socket, factory)
    channel.property("family") = family
    socket.bind(address)
    channel
  }

  /**
    * @return a `UDPChannel[OUT,IN]` that sends outputs of the form `Datagram(OUT)` from a
    *         local datagram socket to the given `address`, and will receive datagrams
    *         only from that address.
    * @param address
    * @param factory builds the typed channel from the (untyped) datagram socket
    * @param family
    * @tparam OUT
    * @tparam IN
    */
  def connect[OUT, IN](address: InetSocketAddress, factory: TypedChannelFactory[OUT, IN], family: ProtocolFamily = IPv4): UDPChannel[OUT, IN] = {
    val socket = DatagramChannel.open(family)
    val channel = new UDPChannel[OUT, IN](socket, factory)
    channel.property("family") = family
    socket.connect(address)
    channel
  }



  /**
    * The same as `bind`, but the address is formed from `host` and `port`.  If the port is `0` then an ephemeral
    * port is used. The protocol family is determined by the host address.
    */
  def bind[OUT, IN](host: String, port: Int, factory: TypedChannelFactory[OUT, IN]): UDPChannel[OUT, IN] = {
    val address = InetAddress.getByName(host)
    val family = address match {
      case _: Inet4Address => IPv4
      case _: Inet6Address => IPv6
    }
    bind(new InetSocketAddress(address, port), factory, family)
  }

  /**
    * The same as `connect`, but the address is formed from `host` and `port`.  If the port is `0` then an ephemeral
    * port is used. The protocol family is determined by the host address.
    */
  def connect[OUT, IN](host: String, port: Int, factory: TypedChannelFactory[OUT, IN]): UDPChannel[OUT, IN] = {
    val address = InetAddress.getByName(host)
    val family = address match {
      case _: Inet4Address => IPv4
      case _: Inet6Address => IPv6
    }
    connect(new InetSocketAddress(address, port), factory, family)
  }

  /** Return a channel that listens to a multicast IP address/port */
  def multiConnect[OUT,IN](host: String, port: Int, factory: TypedChannelFactory[OUT, IN]): UDPChannel[OUT, IN] = {
    val address = InetAddress.getByName(host)
    val family = address match {
      case _: Inet4Address => IPv4
      case _: Inet6Address => IPv6
    }
    multiConnect(new InetSocketAddress(address, port), factory, family)
  }

  /** Return a channel that listens to a multicast IP address/port  */
  def multiConnect[OUT, IN](address: InetSocketAddress, factory: TypedChannelFactory[OUT, IN], family: ProtocolFamily = IPv4): UDPChannel[OUT, IN] = {
    val socket = DatagramChannel.open(family)
    val channel = new UDPChannel[OUT, IN](socket, factory)
    channel.property("family") = family
    if (address.getAddress.isMulticastAddress) {
      val networkInterface = NetworkInterface.getByName("lo0") // TODO
      val membershipKey    = socket.join(address.getAddress, networkInterface)
      channel.setOption(SO_REUSEADDR, value = true)
      log.fine(s"MultiConnect hi=$networkInterface, key=$membershipKey")
    }
    socket.bind(address)
    channel
  }

  /**
    * Return a channel that multicasts to a multicast IP address/port
    */
  def multiBind[OUT, IN](address: InetSocketAddress, factory: TypedChannelFactory[OUT, IN], family: ProtocolFamily = IPv4): UDPChannel[OUT, IN] = {
    val socket = DatagramChannel.open(family)
    val channel = new UDPChannel[OUT, IN](socket, factory)
    val networkInterface = NetworkInterface.getByName("lo0") // TODO
    channel.setOption(IP_MULTICAST_IF, networkInterface)
    channel.setOption(SO_REUSEADDR, value = true)
    channel.property("family") = family
    socket.bind(new InetSocketAddress(address.getPort))
    log.fine(s"MultiBind Interface: $networkInterface, Address: $address, Channel: $channel")
    channel
  }

  /**
    * Return a channel that multicasts to a multicast IP address/port
    */
  def multiBind[OUT, IN](host: String, port: Int, factory: TypedChannelFactory[OUT, IN]): UDPChannel[OUT, IN] = {
    val address = InetAddress.getByName(host)
    val family = address match {
      case _: Inet4Address => IPv4
      case _: Inet6Address => IPv6
    }
    multiBind(new InetSocketAddress(address, port), factory, family)
  }
}



/**
  * A typed channel for transmitting and receiving datagrams of type `OUT`, and `IN` respectively.
  * Datagrams to be sent take the form:
  * {{{
  *   Datagram(out: OUT, destination: SocketAddress = null)
  * }}}
  * then encoded as their wire representations and sent to `destination`.
  * If `destination` is (unspecified or) null, then it is taken to be the address, if
  * any, to which the channel is currently connected. If the channel is not currently
  * connected then an error is thrown.
  *
  * Received datagrams are delivered in one of the forms:
  * {{{
  *  Datagram(in: IN, source: SocketAddress)
  *  Malformed(source: SocketAddress)
  * }}}
  * In the former case, the `IN` is the result of decoding a datagram arriving in wire representation
  * from `source`. The latter case is a notification that an arriving datagram (from source) has not been
  * in the wire encoding declared for this channel, or has been truncated en-route.
  *
  * WARNING: the java datagram channel interface specifies that if a received datagram is too long
  * for the buffer space allocated for it then the excess length of the datagram is "silently discarded".
  *
  * It is therefore essential that adequately sized read-buffers be allocated; or that datagrams carry
  * an indication of their expected length early in their wire representation so that the "silent discarding"
  * can be detected by the `Decoder.decode()` used to decode treceived packets.
  */
class UDPChannel[OUT,IN](val channel:  DatagramChannel, factory: TypedChannelFactory[OUT, IN])  extends
      TypedUDPChannel[UDP[OUT],UDP[IN]] {
  import UDPChannel._

  /** The options set on the underlying channel */
  def options: String = "RCV: %d, SND: %d, REUSE: %b, MULTICSTIF: %s".format(
    getOption(SO_RCVBUF),
    getOption(SO_SNDBUF),
    getOption(SO_REUSEADDR),
    getOption(IP_MULTICAST_IF)
  )

  /** Disconnect this channel from the address, if any, to which it is currently
    * connected, then connect it to the given remote address
    */
  def connect(addr: InetSocketAddress): Unit = {
    if (channel.isConnected) channel.disconnect()
    channel.connect(addr)
    if (logging) finest(s"connected: $this to $addr")
  }

  val output: DatagramOutputStream    = new DatagramOutputStream(channel, ChannelOptions.outSize)
  val input:  DatagramInputStream     = DatagramInputStream(channel, ChannelOptions.inSize)
  val codec:  Codec[OUT,IN]           = factory.newCodec(output, input)

  /** Encode and send the packet using the channel, if it is a Datagram with an address.
    * If the datagram address is null, then the packet is sent
    * to the currently-connected remote address, if any.
    * If the packet is `Malformed` then it is ignored: such packets usually arise
    * as results of arriving datagrams that cannot be decoded.
    */
  def encode(packet: UDP[OUT]): Unit = {
    if (logging) finest(s"before encode(#{$packet.value.size}) [${output.buffer}]")
    packet match {
      case Datagram(value, addr) =>
        output.newDatagram(addr)
        codec.encode (value)
        if (logging) finest (s"after encode($packet) [${output.buffer}]")
      case Malformed(addr) =>
    }
    ()
  }

  /**
    * Await the next raw datagram packet on the channel then decode and
    * return `Datagram(decoded-raw-data, sourceAddress)`.
    *
    * If the packet is malformed because of a decoding failure caused by
    * an incompletely-received datagram then `Malformed(addr)` is returned.
    *
    * If the channel is connected, then another reason for failure might
    * be that the address to which it is connected is unreachable. This
    * causes an `EndOfInputStream` exception to be thrown. The
    * exception will be treated *as if* the channel was closed; except
    * that its `lastException` will retain the  `PortUnreachableException`
    * by which the unreachability was detected.
    */
   def decode(): UDP[IN] = {
     try {
       val sourceAddress = input.receive()
       if (logging) finest(s"decoding ${input})")
       try {
         val value: IN = codec.decode()
         input.clear()
         val result = Datagram(value, sourceAddress)
         if (logging) finest(s"decoded (${input})=$result")
         result
       } catch {
         case exn: EOFException =>
           warning(s"datagram decode failed (abbreviated) (${exn}) [$input]")
           Malformed(sourceAddress)
         case exn: UTFDataFormatException =>
           warning(s"datagram decode failed (UTF8 data malformed) (${exn}) [$input]")
           Malformed(sourceAddress)
       }
     } catch {
       case exn: PortUnreachableException =>
         lastException = exn
         inOpen = false
         throw new EndOfInputStream(input)
     }
   }

   def closeOut(): Unit = output.close()
   def closeIn(): Unit  = input.close()

   /** The most recent exception */
   private var _lastException: Option[Throwable] = None
   private def lastException_=(throwable: Throwable): Unit = _lastException=Some(throwable)
   @inline private def resetLastException(): Unit = _lastException = None

   /** The most recent exception. Set by a channel failure  */
   def lastException: Option[Throwable] = _lastException
   var inOpen = true

   /**
     * The most recent `decode` yielded a valid result if true; else the associated channel closed/failed,
     * and `lastException` may explain why.
     */
   override def canDecode: Boolean = inOpen


  override
  def toString: String = s"UDPChannel[$options] [LastException: $lastException])"


}

/*
class NetMulticastChannel(_channel:  DatagramChannel) extends
  NetDatagramChannel(_channel) with MulticastConnector
{
  /** Join the multicast group at `address` */
  def join(group: InetAddress): MembershipKey =
  { channel.join(group, ni)
  }

  def join(group: InetAddress, source: InetAddress): MembershipKey =
  { channel.join(group, ni, source)
  }

  /** The network interface */
  private
  var ni: NetworkInterface = null.asInstanceOf[NetworkInterface]

  def setNI(interface: NetworkInterface): Unit =
  { ni = interface
    channel.setOption[NetworkInterface](IP_MULTICAST_IF, ni)
  }

  def getNI: NetworkInterface = ni

  override
  def toString: String = "NetMulticastChannel(%s) (%s, NI: %s)".format(channel.toString, options, ni)
}
*/
