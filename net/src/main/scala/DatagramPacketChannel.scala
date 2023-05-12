package ox.net

import io.SourceLocation._
import io.threadcso._
import ox.eieio.options._
import ox.eieio.types._

import java.net._
import java.nio.channels._

object DatagramPacketChannel extends ox.logging.Log("DatagramPacketChannel")
{
  def bound[OUT,IN](address: InetSocketAddress, factory: TypedChannelFactory[OUT, IN]): DatagramPacketChannel[OUT,IN] =
  { val socket = DatagramChannel.open()
    val channel = new DatagramPacketChannel[OUT,IN](socket, factory)
    socket.bind(address)
    channel
  }

  def connected[OUT,IN](address: InetSocketAddress, factory: TypedChannelFactory[OUT, IN]): DatagramPacketChannel[OUT,IN] =
  { val socket = DatagramChannel.open()
    val channel = new DatagramPacketChannel[OUT,IN](socket, factory)
    socket.connect(address)
    channel
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

case class Packet[T](value: T, address: SocketAddress)

class DatagramPacketChannel[OUT,IN](val channel:  DatagramChannel, factory: TypedChannelFactory[OUT, IN])
     extends ChannelProxy[Packet[OUT],Packet[IN]]
     with    HostDatagramChannel
 {
  import DatagramPacketChannel._
  import ox.eieio.options._



  /** The options set on the underlying channel */
  def options: String = "RCV: %d, SND: %d, REUSE: %b, MULTICSTIF: %s".format(
    channel.getOption(SO_RCVBUF),
    channel.getOption(SO_SNDBUF),
    channel.getOption(SO_REUSEADDR),
    channel.getOption(IP_MULTICAST_IF)
  )

  val output: ByteBufferOutputStream = new ByteBufferOutputStream(8192)
  val input: ByteBufferInputStream   = ByteBufferInputStream(8192)
  val codec: Codec[OUT,IN]           = factory.newCodec(output, input)

  def encode(packet: Packet[OUT]): Unit = {
    codec.encode(packet.value)
    send(output.buffer, packet.address)
    output.reuse()
    ()
  }


   def decode(): Packet[IN] = {
    val bb   = input.byteBuffer
    val addr = receive(bb)
    fine(s"decoding ${bb})")
    val value: IN = codec.decode()
    input.reuse()
    fine(s"decoded (${bb})=$value")
    Packet(value, addr)
  }

   /** The most recent exception */
   private var _lastException: Option[Throwable] = None
   private def lastException_=(throwable: Throwable): Unit = _lastException=Some(throwable)
   @inline private def resetLastException(): Unit = _lastException = None

   /** The most recent exception. Set by a channel failure  */
   def lastException: Option[Throwable] = _lastException
   var outOpen, inOpen = true

   /**
     * The most recent `decode` yielded a valid result if true; else the associated channel closed/failed,
     * and `lastException` may explain why.
     *
     */
   override def canDecode: Boolean = inOpen

   /**
     * The most recent `encode` was successful if true; else the associated channel closed/failed,
     * and `lastException` may explain why.
     *
     */
   override def canEncode: Boolean = outOpen

  override
  def toString: String = s"DatagramPacketChannel($channel) [$options] [LastException: $lastException])"

  /** Low-level send of datagram `buffer` to `address`.
    * Yields the number of bytes actually sent; or -1 if the channel effectively closed
    * or the packet was rejected.
    * The variable `lastException: Throwable` can be used to find out exactly what went wrong
    * if
    */
  def send(buffer: BUFFER, address: SocketAddress): Int =
  { resetLastException()
    if (channel.isOpen)
    {
      try
        { val n = channel.send(buffer, address)
        if (n<0) outOpen = false
        n
      }
      catch
      { case exn: java.lang.Exception =>
        lastException = exn
        outOpen = false
        -1
      }
    }
    else {
      outOpen = false
      -1
    }
  }

  /** Low-level receive of datagram `buffer` from the socket to which
    * this datagram channel is connected.
    * Yields null with !canDecode() if something went wrong
    */
  def receive(buffer: BUFFER): SocketAddress = {
    resetLastException()
    buffer.clear
    try {
      val addr = channel.receive(buffer)
      finest(s"received $buffer from $addr")
      buffer.flip
      addr
    } catch {
      case exn: java.lang.Throwable =>
        lastException = exn
        inOpen = false
        null
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
