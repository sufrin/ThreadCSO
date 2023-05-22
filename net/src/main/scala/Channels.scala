package ox.net

import ox.net.codec.Codec

import java.io.{InputStream, OutputStream}
import java.lang
import java.net.{NetworkInterface, ProtocolFamily, Socket, SocketAddress}
import java.nio.channels.{DatagramChannel, SocketChannel}

/**
  * A monstrous mass of boilerplate to overcome the mismatch
  * between java generics and scala generics. The key goal here
  * is to be able to use the `java.net.StandardSocketOption` names, and
  * to ensure that when setting a (java channel) option that indicates
  * that it has a certain type the value provided is of the right type.
  *
  * Java generics of a specific type are injected into marker case classes,
  * from which they can be projected. There may well be better
  * ways to do this with Scala type classes, but I haven't
  * considered them yet.
  *
  */
object SocketOptions {
  trait SocketOption[T] {
    val netOption:   java.net.SocketOption[_ <: Any]
    val toBoolean:   java.net.SocketOption[java.lang.Boolean] = netOption.asInstanceOf[java.net.SocketOption[java.lang.Boolean]]
    val toInt:       java.net.SocketOption[java.lang.Integer] = netOption.asInstanceOf[java.net.SocketOption[java.lang.Integer]]
    val toInterface: java.net.SocketOption[NetworkInterface]  = netOption.asInstanceOf[java.net.SocketOption[NetworkInterface]]
  }

  case class IntOption(netOption: java.net.SocketOption[lang.Integer]) extends SocketOption[scala.Int]
  case class BoolOption(netOption: java.net.SocketOption[java.lang.Boolean]) extends SocketOption[scala.Boolean]
  case class InterfaceOption(netOption: java.net.SocketOption[NetworkInterface]) extends SocketOption[NetworkInterface]

  val SO_RCVBUF     = IntOption(java.net.StandardSocketOptions.SO_RCVBUF)
  val SO_SNDBUF     = IntOption(java.net.StandardSocketOptions.SO_SNDBUF)
  val SO_KEEPALIVE  = BoolOption(java.net.StandardSocketOptions.SO_KEEPALIVE)
  val SO_REUSEADDR  = BoolOption(java.net.StandardSocketOptions.SO_REUSEADDR)
  val SO_BROADCAST  = BoolOption(java.net.StandardSocketOptions.SO_BROADCAST)
  val TCP_NODELAY   = BoolOption(java.net.StandardSocketOptions.TCP_NODELAY)
  val IP_TOS        = IntOption(java.net.StandardSocketOptions.IP_TOS)
  val IP_MULTICAST_IF   = InterfaceOption(java.net.StandardSocketOptions.IP_MULTICAST_IF)
  val IP_MULTICAST_TTL  = IntOption(java.net.StandardSocketOptions.IP_MULTICAST_TTL)
  val IP_MULTICAST_LOOP = BoolOption(java.net.StandardSocketOptions.IP_MULTICAST_LOOP)

  val IPv4: java.net.StandardProtocolFamily = java.net.StandardProtocolFamily.INET
  val IPv6: java.net.StandardProtocolFamily = java.net.StandardProtocolFamily.INET6
}

/**
  * Dynamically-set channel features that are bound into network channels at the
  * point of their construction.
  */
object ChannelOptions {
  val IPv4: java.net.StandardProtocolFamily = java.net.StandardProtocolFamily.INET
  val IPv6: java.net.StandardProtocolFamily = java.net.StandardProtocolFamily.INET6

  var inSize:  Int                    = 8*1024
  var outSize: Int                    = 8*1024
  var protocolFamily: ProtocolFamily  = IPv4
  var sync: Boolean = true
  /**
    * Define a selection of features while constructing a
    * channel (or, indeed, any value)
    *
    * @param inSize size of preallocated input buffers. Affects efficiency, but not correctness.
    *               Experiments show that it can be as small as 1! But there is generally an expensive  switch of
    *               kernel context at each read.
    * @param outSize size of preallocated output buffers. For the moment this should be no smaller than the size of the largest
    *                message to be sent on the channel. (TODO: apply the easy but non-urgent fix)
    * @param sync whether the channel is "synchronous", ie. intermediate streams are flushed immediately after writes.
    *             When false there may be a delay between the logical (CSO) write to a network channel, and its realization
    *             as a network write. The delay makes for more efficient use of buffers and network capacity, but is not
    *             appropriate in some situations
    * @param protocolFamily IPv4 or IPv6
    */
  def withOptions[T](protocolFamily: ProtocolFamily = this.protocolFamily,
                     inSize:         Int=this.inSize,
                     outSize:        Int=this.outSize,
                     sync:           Boolean = this.sync)(makechannel: => T): T = synchronized {
      val is = this.inSize
      val os = this.outSize
      val pf = this.protocolFamily
      val sy = this.sync
      this.inSize         = inSize
      this.outSize        = outSize
      this.protocolFamily = protocolFamily
      this.sync = sync
      try makechannel finally {
        this.inSize         = is
        this.outSize        = os
        this.protocolFamily = pf
        this.sync           = sy
      }
  }
}

/**
  * All ox.net.Channel implementations have this interface in common.
  * The details of an implementation depend on the type of transport it uses.
  * At present this may be an `nio.SocketChannel`, an `nio.DatagramChannel`, or a `net.Socket`.
  */
trait ChannelInterface {
  import SocketOptions.SocketOption
  def getRemoteAddress: SocketAddress
  def setOption(opt: SocketOption[Int], value: Int): Unit
  def setOption(opt: SocketOption[Boolean], value: Boolean): Unit
  def setOption(opt: SocketOption[NetworkInterface], value: NetworkInterface): Unit
  def getOption(opt: SocketOption[Int]): Int
  def getOption(opt: SocketOption[Boolean]): Boolean
  def getOption(opt: SocketOption[NetworkInterface]): NetworkInterface
  /** Close the socket completely */
  def close(): Unit
  /** Close the input side of the socket */
  def shutdownInput(): Unit
  /** Close the output side of the socket */
  def shutdownOutput(): Unit
  /** Property table for "experimental" features */
  val property = new collection.mutable.HashMap[String, Any]
}

trait TCPChannelInterface extends ChannelInterface {
  import SocketOptions.SocketOption
  val channel: SocketChannel
  def getRemoteAddress: SocketAddress = channel.getRemoteAddress
  def setOption(opt: SocketOption[Int], value: Int): Unit = channel.setOption(opt.toInt, value.asInstanceOf[Integer])
  def setOption(opt: SocketOption[Boolean], value: Boolean): Unit = channel.setOption(opt.toBoolean, if (value) java.lang.Boolean.TRUE else java.lang.Boolean.FALSE)
  def setOption(opt: SocketOption[NetworkInterface], value: NetworkInterface): Unit = channel.setOption(opt.toInterface, value)
  def getOption(opt: SocketOption[Int]): Int = channel.getOption(opt.toInt)
  def getOption(opt: SocketOption[Boolean]): Boolean = channel.getOption(opt.toBoolean)
  def getOption(opt: SocketOption[NetworkInterface]): NetworkInterface = channel.getOption(opt.toInterface)
  /** Close the socket completely */
  def close(): Unit = channel.close()
  /** Close the input side of the socket */
  def shutdownInput(): Unit = if (channel.isOpen) channel.shutdownInput()
  /** Close the output side of the socket */
  def shutdownOutput(): Unit = if (channel.isOpen) channel.shutdownOutput()
}

trait UDPChannelInterface extends ChannelInterface {
  import SocketOptions.SocketOption
  val channel: DatagramChannel
  def getRemoteAddress: SocketAddress = channel.getRemoteAddress
  def setOption(opt: SocketOption[Int], value: Int): Unit = channel.setOption(opt.toInt, value.asInstanceOf[Integer])
  def setOption(opt: SocketOption[Boolean], value: Boolean): Unit = channel.setOption(opt.toBoolean, if (value) java.lang.Boolean.TRUE else java.lang.Boolean.FALSE)
  def setOption(opt: SocketOption[NetworkInterface], value: NetworkInterface): Unit = channel.setOption(opt.toInterface, value)
  def getOption(opt: SocketOption[Int]): Int = channel.getOption(opt.toInt)
  def getOption(opt: SocketOption[Boolean]): Boolean = channel.getOption(opt.toBoolean)
  def getOption(opt: SocketOption[NetworkInterface]): NetworkInterface = channel.getOption(opt.toInterface)
  /** Close the socket completely */
  def close(): Unit = channel.close()
  /**  No-op for datagram channelfactory */
  def shutdownInput(): Unit = {}
  /** No-op for datagram channelfactory */
  def shutdownOutput(): Unit = {}
}

trait SSLChannelInterface extends ChannelInterface {
  import SocketOptions.SocketOption
  val  socket: Socket
  def getRemoteAddress: SocketAddress = socket.getRemoteSocketAddress()
  def setOption(opt: SocketOption[Int], value: Int): Unit = socket.setOption(opt.toInt, value.asInstanceOf[Integer])
  def setOption(opt: SocketOption[Boolean], value: Boolean): Unit = socket.setOption(opt.toBoolean, if (value) java.lang.Boolean.TRUE else java.lang.Boolean.FALSE)
  def setOption(opt: SocketOption[NetworkInterface], value: NetworkInterface): Unit = socket.setOption(opt.toInterface, value)
  def getOption(opt: SocketOption[Int]): Int = socket.getOption(opt.toInt)
  def getOption(opt: SocketOption[Boolean]): Boolean = socket.getOption(opt.toBoolean)
  def getOption(opt: SocketOption[NetworkInterface]): NetworkInterface = socket.getOption(opt.toInterface)

  /** Close the socket completely */
  def close(): Unit = if (!socket.isClosed) socket.close()
  /** Close the input side of the socket */
  def shutdownInput(): Unit = close()
  /** Close the output side of the socket */
  def shutdownOutput(): Unit = close()
}

trait TypedTCPChannel[OUT,IN] extends NetProxy[OUT,IN] with TCPChannelInterface
trait TypedUDPChannel[OUT,IN] extends NetProxy[OUT,IN] with UDPChannelInterface
trait TypedSSLChannel[OUT,IN] extends NetProxy[OUT,IN] with SSLChannelInterface

trait TypedChannelFactory[OUT,IN] {
  /** Build a `NetProxy`` from the given `SocketChannel` */
  def newChannel(channel: SocketChannel):  TypedTCPChannel[OUT,IN]
  /**
    * Build a `NetProxy`` from the given `Socket`
    * Expected to be used only for SSL/TLS Sockets.
    */
  def newChannel(socket: java.net.Socket): TypedSSLChannel[OUT,IN]

  /**
    * Build a `Codec` from the given `input` and `output` streams.
    */
  def newCodec(output: OutputStream, input: InputStream): Codec[OUT,IN]
}

