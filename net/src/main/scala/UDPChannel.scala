package ox.net

import io.SourceLocation._
import ox.net.SocketOptions._
import ox.net.UDPChannel.UDP
import ox.net.codec.Codec

import java.io.{EOFException, UTFDataFormatException}
import java.net._
import java.nio.ByteBuffer
import java.nio.channels._

object UDPChannel extends ox.logging.Log("UDPChannel")
{

  sealed trait UDP[T] {
    val address: SocketAddress
  }

  case class Malformed[T](address: SocketAddress) extends UDP[T]
  case class Datagram[T](value: T, address: SocketAddress) extends UDP[T] {
    override def toString: String =
      value match {
        case s: String => if (s.length > 20) s"${s.substring(0, 20)}...#${s.length}" else s
        case _ => value.toString
      }

  }

  /**
    * @return a `UDPChannel[OUT,IN]` that sends outputs of the form `Datagram(OUT, destinationAddress)` from a
    *         datagram socket at the given `address`.
    * @param address
    * @param factory builds the typed channel from the (untyped) tagaram socket
    * @param family
    * @tparam OUT
    * @tparam IN
    */
  def bind[OUT,IN](address: InetSocketAddress, factory: TypedChannelFactory[OUT, IN], family: ProtocolFamily = IPv4): UDPChannel[OUT,IN] =
  { val socket = DatagramChannel.open(family)
    val channel = new UDPChannel[OUT,IN](socket, factory)
    channel.property("family") = family
    socket.bind(address)
    channel
  }

  /**
    * @return a `UDPChannel[OUT,IN]` that listens for inputs of the form `Datagram(OUT, sourceAddress)` from a
    *         datagram socket at the given `address`.
    * @param address
    * @param factory builds the typed channel from the (untyped) tagaram socket
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
  /*
  def multicast(address: InetSocketAddress): NetMulticastChannel =
    multicast(address, address.getAddress)

  def multicast(address: InetSocketAddress, interface: InetAddress): NetMulticastChannel =
  { val channel = new NetMulticastChannel(DatagramChannel.open(options.IPv4))
    val ni      = NetworkInterface.getByInetAddress(interface)
    channel.setOption(SO_REUSEADDR, value = true)
    channel.bind(address)
    channel.setNI(ni)
    channel
  }
  */

}



/**
  * A channel for transmitting and receiving datagrams.
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

  val output: ByteBufferOutputStream  = new ByteBufferOutputStream(ChannelOptions.outSize)
  val input:  ByteBufferInputStream   = ByteBufferInputStream(ChannelOptions.inSize)
  val codec:  Codec[OUT,IN]           = factory.newCodec(output, input)

  def encode(packet: UDP[OUT]): Unit = {
    output.reuse()
    if (logging) finest(s"before encode(#{$packet.value.size}) [${output.buffer}]")
    packet match {
      case Datagram(value, addr) =>
        codec.encode (value)
        if (logging) finest (s"after encode($packet) [${output.buffer}]")
        send (output.buffer, packet.address)
      case Malformed(addr) =>

    }
    ()
  }

   def decode(): UDP[IN] = {
    val bb   = input.byteBuffer
    val addr = receive(bb)
    if (logging) finest(s"decoding ${bb})")
     try {
       val value: IN = codec.decode()
       input.reuse()
       val result = Datagram(value, addr)
       if (logging) finest(s"decoded (${bb})=$result")
       result
     } catch  {
       case exn: EOFException =>
            warning(s"datagram decode failed (abbreviated) (${exn}) [$input]")
            Malformed(addr)
       case exn: UTFDataFormatException =>
            warning(s"datagram decode failed (UTF8 data malformed) (${exn}) [$input]")
            Malformed(addr)
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
     *
     */
   override def canDecode: Boolean = inOpen


  override
  def toString: String = s"UDPChannel($channel) [$options] [LastException: $lastException])"

  /** Low-level send of datagram `buffer` to `address`.
    * Yields the number of bytes actually sent; or -1 if the channel effectively closed
    * or the packet was rejected.
    * The variable `lastException: Throwable` can be used to find out exactly what went wrong
    * if
    */
  def send(buffer: ByteBuffer, address: SocketAddress): Int =
  { resetLastException()
    var count = 0
    try {
      if (channel.isOpen) {
        buffer.flip()
        if (logging) finest(s"UDP.send($buffer)")
        while (buffer.hasRemaining())
          count += channel.send(buffer, address)
        finest(s"UDP.sent($count)")
        count
      }
      else {
        -1
      }
    } catch  {
      case exn: java.lang.Throwable =>
           if (logging) fine(s"UDP.send threw $exn ")
           lastException = exn
           -1
    }
  }

  /** Low-level receive of datagram `buffer` from the socket to which
    * this datagram channel is connected.
    * Yields null with !canDecode() if something went wrong
    */
  def receive(buffer: ByteBuffer): SocketAddress = {
    resetLastException()
    buffer.clear
    try {
      val addr = channel.receive(buffer)
      if (logging) finest(s"received $buffer from $addr")
      buffer.flip
      addr
    } catch {
      case exn: java.lang.Throwable =>
        lastException = exn
        inOpen = false
        throw exn
    }
  }
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
