package ox.net.channelfactory

import org.velvia.InvalidMsgPackDataException
import org.velvia.msgpack.{Codec => VelviaCodec}
import ox.net.TypedChannelFactory
import ox.net.TypedSSLChannel
import ox.net.TypedTCPChannel

import java.io._
import java.net.Socket
import java.nio.channels.SocketChannel

/**
  * An abstract `ChannelFactory` for a pair of types, `[OUT,IN]` each with an  `org.velvia.msgpack.Codec` stream-encoding that can be synthesised or
  * inferred (for example by using the tools of `org.velvia.msgpack`).
  *
  * This class is structurally almost the same as `StreamerChannelFactory`, and no doubt we could find an
  * abstraction that covers them both if we tried hard enough.
  *
  * @see  StreamerChannelFactory, CRLFChannelFactory, UTF8ChannelFactory
  */

object VelviaChannelFactory {
  val log = new ox.logging.Log()
}

class VelviaChannelFactory[OUT : VelviaCodec, IN: VelviaCodec] extends TypedChannelFactory[OUT, IN] {
  val log = VelviaChannelFactory.log

  def newChannel(theChannel: SocketChannel): TypedTCPChannel[OUT,IN] = new TypedTCPChannel[OUT, IN] with Mixin {
    val channel = theChannel
    val output = new DataOutputStream(new BufferedOutputStream(java.nio.channels.Channels.newOutputStream(channel)))
    val input = new DataInputStream(new BufferedInputStream(java.nio.channels.Channels.newInputStream(channel)))
  }

  /**
    * Build a `NetProxy`` from the given `Socket`
    * Expected to be used only for SSL/TLS Sockets.
    */
  def newChannel(theSocket: Socket): TypedSSLChannel[OUT, IN] = new TypedSSLChannel[OUT, IN] with Mixin {
    val socket = theSocket
    val output = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream))
    val input = new DataInputStream(new BufferedInputStream(socket.getInputStream))
  }

  /**
    * Build a `Codec` from the given `input` and `output` streams.
    */
  def newCodec(_output: OutputStream, _input: InputStream): ox.net.codec.Codec[OUT, IN] = new ox.net.codec.Codec[OUT,IN] with Mixin {
    val output = new DataOutputStream(new BufferedOutputStream(_output))
    val input = new DataInputStream(new BufferedInputStream(_input))
  }

  trait Mixin {
    val input: DataInputStream
    val output: DataOutputStream
    var inOpen, outOpen = true
    def sync: Boolean

    def decode(): IN = try {
      org.velvia.MessagePack.unpack(input)(implicitly[VelviaCodec[IN]])
    } catch {
      case exn: InvalidMsgPackDataException => inOpen = false; throw new ox.net.codec.EndOfInputStream(input)
      case exn: UTFDataFormatException => inOpen = false; throw new ox.net.codec.EndOfInputStream(input)
      case exn: EOFException => inOpen = false; throw new ox.net.codec.EndOfInputStream(input)
      case exn: IOException => inOpen = false; throw new ox.net.codec.EndOfInputStream(input)
    }

    /**
      * The most recent `fromStream` yielded a valid result if true;
      * else the associated stream closed or the fromStream failed.
      */
    def canDecode: Boolean = inOpen

    /**
      * Stop decoding and release/close any engaged resources,
      * including the associated stream.
      */
    def closeIn(): Unit = input.close()

    /**
      * Encode `output` and transmit its representation to
      * the associated network stream
      */
    def encode(value: OUT): Unit = {
      org.velvia.MessagePack.pack(value, output)
      if (sync) output.flush()
    }

    /**
      * Stop encoding and release/close any engaged resources,
      * including the associated stream.
      */
    def closeOut(): Unit = output.close()
  }
}

