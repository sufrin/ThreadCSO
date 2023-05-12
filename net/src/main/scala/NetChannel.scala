package ox.net

import io.threadcso.process.PROC
import io.threadcso.{!!, ??, proc, repeat}
import ox.net.CRLFChannelFactory.{CRLFMixin, UTF8}
import ox.net.NetOptions.Option

import java.io._
import java.lang
import java.net.{NetworkInterface, Socket, SocketAddress, SocketOption}
import java.nio.channels.{DatagramChannel, SocketChannel}

trait Encoder[O] {
  private var _sync: Boolean = true

  /** encode `output` and transmit it to the associated network channel */
  def encode(output: O): Unit

  /** The most recent `encode` was successful if true; else the associated channel closed */
  def canEncode: Boolean

  /** The associated channel should be flushed after an encode */
  def sync: Boolean = _sync
  /** Set sync */
  def sync_=(sync: Boolean) = _sync = sync
}

trait Decoder[I] {
  /** decode the next encoded item on the associated network channel */
  def decode(): I

  /** The most recent `decode` yielded a valid result if true; else the associated channel closed */
  def canDecode: Boolean
}

trait Codec[O,I] extends Encoder[O] with Decoder[I] {
}

/**
  * A monstrous regiment of Hacks that might, perhaps, overcome the mismatch
  * between java generics and scala generics
  */
object NetOptions {
  trait Option[T] {
    val netOption: SocketOption[_ <: Any]
    val boolOption: SocketOption[java.lang.Boolean] = netOption.asInstanceOf[SocketOption[java.lang.Boolean]]
    val intOption: SocketOption[java.lang.Integer] = netOption.asInstanceOf[SocketOption[java.lang.Integer]]
    val niOption: SocketOption[NetworkInterface] = netOption.asInstanceOf[SocketOption[NetworkInterface]]
  }
  case class IntOption(netOption: SocketOption[lang.Integer]) extends Option[scala.Int]
  case class BoolOption(netOption: SocketOption[java.lang.Boolean]) extends Option[scala.Boolean]
  case class NIOption(netOption: SocketOption[NetworkInterface]) extends Option[NetworkInterface]

  val SO_RCVBUF = IntOption(java.net.StandardSocketOptions.SO_RCVBUF)
  val SO_SNDBUF = IntOption(java.net.StandardSocketOptions.SO_SNDBUF)
  val SO_KEEPALIVE = BoolOption(java.net.StandardSocketOptions.SO_KEEPALIVE)
  val SO_REUSEADDR = BoolOption(java.net.StandardSocketOptions.SO_REUSEADDR)
  val SO_BROADCAST = BoolOption(java.net.StandardSocketOptions.SO_BROADCAST)
  val TCP_NODELAY = BoolOption(java.net.StandardSocketOptions.TCP_NODELAY)
  val IP_TOS = IntOption(java.net.StandardSocketOptions.IP_TOS)
  val IP_MULTICAST_IF = NIOption(java.net.StandardSocketOptions.IP_MULTICAST_IF)
  val IP_MULTICAST_TTL = IntOption(java.net.StandardSocketOptions.IP_MULTICAST_TTL)
  val IP_MULTICAST_LOOP = BoolOption(java.net.StandardSocketOptions.IP_MULTICAST_LOOP)
  val IPv4 = java.net.StandardProtocolFamily.INET
  val IPv6 = java.net.StandardProtocolFamily.INET6
}

trait HostChannelInterface {
  import NetOptions.Option
  def getRemoteAddress: SocketAddress
  def setOption(opt: Option[Int], value: Int): Unit
  def setOption(opt: Option[Boolean], value: Boolean): Unit
  def getOption[T](opt: Option[T]): T
  /** Close the socket completely */
  def close(): Unit
  /** Close the input side of the socket */
  def shutdownInput(): Unit
  /** Close the output side of the socket */
  def shutdownOutput(): Unit
}

trait HostSocketChannel extends HostChannelInterface {
  import NetOptions.Option
  val channel: SocketChannel
  def getRemoteAddress: SocketAddress = channel.getRemoteAddress
  def setOption(opt: Option[Int], value: Int): Unit = channel.setOption(opt.intOption, value.asInstanceOf[Integer])
  def setOption(opt: Option[Boolean], value: Boolean): Unit = channel.setOption(opt.boolOption, if (value) java.lang.Boolean.TRUE else java.lang.Boolean.FALSE)
  def getOption[T](opt: Option[T]): T = channel.getOption(opt.netOption).asInstanceOf[T]
  /** Close the socket completely */
  def close(): Unit = channel.close()
  /** Close the input side of the socket */
  def shutdownInput(): Unit = if (channel.isOpen) channel.shutdownInput()
  /** Close the output side of the socket */
  def shutdownOutput(): Unit = if (channel.isOpen) channel.shutdownOutput()
}

trait HostDatagramChannel extends HostChannelInterface {
  import NetOptions.Option
  val channel: DatagramChannel
  def getRemoteAddress: SocketAddress = channel.getRemoteAddress
  def setOption(opt: Option[Int], value: Int): Unit = channel.setOption(opt.intOption, value.asInstanceOf[Integer])
  def setOption(opt: Option[Boolean], value: Boolean): Unit = channel.setOption(opt.boolOption, if (value) java.lang.Boolean.TRUE else java.lang.Boolean.FALSE)
  def getOption[T](opt: Option[T]): T = channel.getOption(opt.netOption).asInstanceOf[T]
  /** Close the socket completely */
  def close(): Unit = channel.close()
  /**  No-op for datagram channels */
  def shutdownInput(): Unit = {}
  /** No-op for datagram channels */
  def shutdownOutput(): Unit = {}
}

trait HostSocket extends HostChannelInterface {
  val  socket: Socket

  def getRemoteAddress: SocketAddress = socket.getRemoteSocketAddress()
  def setOption(opt: Option[Int], value: Int): Unit = ???
  def setOption(opt: Option[Boolean], value: Boolean): Unit = ???
  def getOption[T](opt: Option[T]): T = ???

  /** Close the socket completely */
  def close(): Unit = if (!socket.isClosed) socket.close()
  /** Close the input side of the socket */
  def shutdownInput(): Unit = close()
  /** Close the output side of the socket */
  def shutdownOutput(): Unit = close()
}



trait OutputProxy[O] extends Encoder[O] {
  def CopyToNet(in: ??[O]): PROC = proc(this.toString + ".CopyToNet") {
    var going = canEncode
    repeat(going) {
      val value = in ? ()
      encode(value)
      going = canEncode
    }
    if (!going) in.closeIn()
  }
}

trait InputProxy[I] extends Decoder[I] {
  def CopyFromNet(out: !![I]): PROC = proc(this.toString + ".CopyFromNet") {
    var going = canDecode
    try {
      repeat(going) {
        val decoded = decode()
        going = canDecode
        if (going) {
          out ! decoded
        }
      }
      if (!going) out.closeOut()
    } catch {
      case exn: java.nio.channels.ClosedByInterruptException =>
        // log.fine(s"CopyFromNet terminated by ClosedInterrupt")
        out.closeOut()
    }
  }
}

trait ChannelProxy[O,I]            extends OutputProxy[O] with InputProxy[I]

trait TypedSocketChannel[OUT,IN]   extends ChannelProxy[OUT,IN] with HostSocketChannel
trait TypedDatagramChannel[OUT,IN] extends ChannelProxy[OUT,IN] with HostDatagramChannel
trait TypedSocket[OUT,IN]          extends ChannelProxy[OUT,IN] with HostSocket

trait TypedChannelFactory[OUT,IN] {
  /** Build a `ChannelProxy`` from the given `SocketChannel` */
  def newChannel(channel: SocketChannel):  ChannelProxy[OUT,IN]
  /**
    * Build a `ChannelProxy`` from the given `Socket`
    * Expected to be used only for SSL/TLS Sockets.
    */
  def newChannel(socket: java.net.Socket): ChannelProxy[OUT,IN]

  /**
    * Build a `ChannelProxy`` from the given `input` and `output` streams.
    */
  def newCodec(output: OutputStream, input: InputStream): Codec[OUT,IN]
}

object CRLFChannelFactory extends TypedChannelFactory[String,String] {

  trait CRLFMixin {
    val output: java.io.Writer
    val input:  java.io.BufferedReader
    def sync:   Boolean

    var inOpen, outOpen: Boolean = true

    def canEncode: Boolean = inOpen

    def canDecode: Boolean = outOpen

    def decode(): String = input.readLine() match {
      case null =>
        inOpen = false
        ""
      case string => string
    }

    def encode(value: String): Unit = {
      output.write(value)
      output.write("\r\n")
      if (sync) output.flush()
    }
  }

  val UTF8 = java.nio.charset.Charset.forName("UTF-8")

  def newChannel(theChannel: SocketChannel): ChannelProxy[String, String] = new TypedSocketChannel[String, String] with CRLFMixin {
    val channel: SocketChannel = theChannel
    val output = java.nio.channels.Channels.newWriter(channel, UTF8.newEncoder(), 8192) // TODO: factory parameters
    val input = new BufferedReader(java.nio.channels.Channels.newReader(channel, UTF8.newDecoder(), 8192)) // TODO: factory parameters
  }

  def newChannel(theSocket: Socket): ChannelProxy[String, String] = new TypedSocket[String, String] with CRLFMixin {
    val socket: Socket = theSocket
    val output = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream, UTF8.newEncoder()), 8192) // TODO: factory parameters
    val input = new BufferedReader(new InputStreamReader(socket.getInputStream, UTF8.newDecoder()), 8192) // TODO: factory parameters
  }

  def newCodec(_output: OutputStream, _input: InputStream): Codec[String,String] =
      new Codec[String,String] with CRLFMixin {
        val output = new BufferedWriter(new OutputStreamWriter(_output, UTF8.newEncoder()), 8192)
        val input = new BufferedReader(new InputStreamReader(_input, UTF8.newDecoder()), 8192) // TODO: factory parameters
      }
}

object UTF8ChannelFactory extends TypedChannelFactory[String,String] {

  trait UTF8Mixin {
    val input: InputStream
    val output: OutputStream
    def sync: Boolean

    val out = new java.io.DataOutputStream(output)
    val in = new java.io.DataInputStream(input)
    var outOpen, inOpen: Boolean = true

    def canEncode: Boolean = outOpen

    def encode(value: String): Unit = try {
      out.writeUTF(value)
      if (sync) out.flush()
    } catch {
      case exn: IOException =>
        outOpen = false
    }

    def canDecode: Boolean = inOpen

    def decode(): String = try in.readUTF() catch {
      case exn: EOFException =>
        inOpen = false
        ""
    }
  }

  def newChannel(theChannel: SocketChannel): ChannelProxy[String, String] = new TypedSocketChannel[String, String] with UTF8Mixin {
    val channel: SocketChannel = theChannel
    val input: InputStream = java.nio.channels.Channels.newInputStream(channel)
    val output: OutputStream = java.nio.channels.Channels.newOutputStream(channel)
  }

  def newChannel(theSocket: Socket): ChannelProxy[String, String] = new TypedSocket[String, String] with UTF8Mixin {
    val socket: Socket = theSocket
    val input: InputStream = theSocket.getInputStream
    val output: OutputStream = theSocket.getOutputStream
  }

  def newCodec(_output: OutputStream, _input: InputStream): Codec[String, String] =
    new Codec[String, String] with CRLFMixin {
      val output = new BufferedWriter(new OutputStreamWriter(_output, UTF8.newEncoder()), 8192)
      val input = new BufferedReader(new InputStreamReader(_input, UTF8.newDecoder()), 8192) // TODO: factory parameters
    }

}


object NetChannel {

  def bound[OUT, IN](address: SocketAddress, factory: TypedChannelFactory[OUT,IN]): ChannelProxy[OUT, IN] = {
    val socket = SocketChannel.open
    val channel = factory.newChannel(SocketChannel.open)
    socket.bind(address)
    channel
  }

  /** Construct a synchronous network channel connected to the given socket address */
  def connected[OUT, IN](address: SocketAddress, factory: TypedChannelFactory[OUT,IN]): ChannelProxy[OUT, IN] = {
    val socket = SocketChannel.open
    val channel = factory.newChannel(socket)
    socket.connect(address)
    channel
  }

}


