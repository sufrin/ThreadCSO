package ox.net

import io.threadcso.process.PROC
import io.threadcso.{!!, ??, proc, repeat}

import java.io._
import java.net.{Socket, SocketAddress, SocketOption}
import java.nio.channels.SocketChannel


trait Codec[O,I] {
  /** encode `output` and transmit it to the associated network channel */
  def encode(output: O): Unit
  /** The most recent `encode` was successful if true; else the associated channel closed */
  def canEncode: Boolean
  /** decode the next encoded item on the associated network channel */
  def decode(): I
  /** The most recent `decode` yielded a valid result if true; else the associated channel closed */
  def canDecode: Boolean

  private var _sync: Boolean = true
  /** The associated channel should be flushed after an encode */
  def sync: Boolean = _sync
  /** Set sync */
  def sync_=(sync: Boolean) = _sync = sync
}

trait SocketChannelInterface {
  val channel: SocketChannel

  def getRemoteAddress: SocketAddress = channel.getRemoteAddress
  def setOption(opt: SocketOption[Integer], value: Int): Unit = channel.setOption(opt, value.asInstanceOf[Integer])
  def setOption(opt: SocketOption[java.lang.Boolean], value: Boolean): Unit = channel.setOption(opt, if (value) java.lang.Boolean.TRUE else java.lang.Boolean.FALSE)
  def setOption[T](opt: SocketOption[T], value: T): Unit = channel.setOption(opt, value)
  def getOption[T](opt: SocketOption[T]): T = channel.getOption(opt)
  /** Close the socket completely */
  def close(): Unit = channel.close()
  /** Close the input side of the socket */
  def shutdownInput(): Unit = if (channel.isOpen) channel.shutdownInput()
  /** Close the output side of the socket */
  def shutdownOutput(): Unit = if (channel.isOpen) channel.shutdownOutput
}

trait ChannelProxy[O,I] extends Codec[O,I]  {
  val log = ox.logging.Log("typedchannel")

  def CopyToNet(in: ??[O]): PROC = proc(this.toString + ".CopyToNet") {
    var going = canEncode
    repeat (going) {
      val value = in ? ()
      encode(value)
      going = canEncode
    }
    if (!going) in.closeIn()
  }

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
        log.fine(s"CopyFromNet terminated by ClosedInterrupt")
        out.closeOut()
    }
  }
}

trait TypedSocketChannel[OUT,IN] extends ChannelProxy[OUT,IN] with SocketChannelInterface
trait TypedSocket[OUT,IN]        extends ChannelProxy[OUT,IN]

trait TypedChannelFactory[OUT,IN] {
  /** Build a `ChannelProxy`` from the given `SocketChannel` */
  def newChannel(channel: SocketChannel):  ChannelProxy[OUT,IN]
  /**
    * Build a `ChannelProxy`` from the given `Socket`
    * Expected to be used only for SSL/TLS Sockets.
    */
  def newChannel(socket: java.net.Socket): ChannelProxy[OUT,IN]
}

object CRLFChannelFactory extends TypedChannelFactory[String,String] {
  val UTF8 = java.nio.charset.Charset.forName("UTF-8")

  def newChannel(theChannel: SocketChannel): ChannelProxy[String, String] = new TypedSocketChannel[String, String] {
    val channel: SocketChannel = theChannel

    val output = java.nio.channels.Channels.newWriter(channel, UTF8.newEncoder(), 8192) // TODO: factory parameters
    val input = new BufferedReader(java.nio.channels.Channels.newReader(channel, UTF8.newDecoder(), 8192)) // TODO: factory parameters
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

  /** For backward compatibility */
  def newChannel(theSocket: Socket): ChannelProxy[String, String] = new TypedSocket[String, String] {
    val socket: Socket = theSocket
    val output = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream, UTF8.newEncoder()), 8192) // TODO: factory parameters
    val input = new BufferedReader(new InputStreamReader(socket.getInputStream, UTF8.newDecoder()), 8192) // TODO: factory parameters
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
}

object UTF8ChannelFactory extends TypedChannelFactory[String,String] {
  def newChannel(theChannel: SocketChannel): ChannelProxy[String, String] = new TypedSocketChannel[String, String] {
    val channel: SocketChannel = theChannel
    val input: InputStream = java.nio.channels.Channels.newInputStream(channel)
    val output: OutputStream = java.nio.channels.Channels.newOutputStream(channel)
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

  def newChannel(theSocket: Socket): ChannelProxy[String, String] = new TypedSocket[String, String] {
    val socket: Socket = theSocket
    val input: InputStream = theSocket.getInputStream
    val output: OutputStream = theSocket.getOutputStream
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


