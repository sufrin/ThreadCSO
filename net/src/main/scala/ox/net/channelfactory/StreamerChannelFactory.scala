package ox.net.channelfactory

import ox.net.codec.StreamerEncoding.Streamer
import ox.net.codec.{Codec, EndOfInputStream}
import ox.net.{TypedChannelFactory, TypedSSLChannel, TypedTCPChannel}

import java.io._
import java.net.Socket
import java.nio.channels.SocketChannel

/**
  * An abstract `ChannelFactory` for a pair of types, `[OUT,IN]` each with a  `Streamer` stream-encoding that can be synthesised or
  * inferred (for example by using the tools of `StreamerEncoding`).
  *
  * @see  VelviaChannelFactory, CRLFChannelFactory, UTF8ChannelFactory for factories that use other (families of) encoding.
  */
class StreamerChannelFactory[OUT : Streamer, IN: Streamer]
       extends ox.logging.Log("StreamerChannelFactory")
       with TypedChannelFactory[OUT, IN] {

  private val tenc = implicitly[Streamer[OUT]]
  private val uenc = implicitly[Streamer[IN]]

  /**
    * This mixin requires `input` and `output` datastreams, and a `sync`
    * to be defined in its host class. It provides `encode(), `decode()`,
    * `canEncode()`, `canDecode()`, etc. to be used in the construction of
    * the typed channels/codecs to be provided by its enclosing `StreamerChannelFactory`.
    */
  trait Mixin {
    val input:  DataInputStream
    val output: DataOutputStream
    var inOpen, outOpen = true
    def sync: Boolean

    /**
      * Decode the next encoded item on the associated network stream
      */
    def decode(): IN = try {
      uenc.fromStream(input)
    } catch {
      case exn: UTFDataFormatException => inOpen = false; throw new EndOfInputStream(input)
      case exn: EOFException => inOpen = false; throw new EndOfInputStream(input)
      case exn: IOException => inOpen = false; throw new EndOfInputStream(input)
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
    def encode(t: OUT): Unit = {
      tenc.toStream(output, t)
      if (sync) output.flush()
    }

    /**
      * Stop encoding and release/close any engaged resources,
      * including the associated stream.
      */
    def closeOut(): Unit = output.close()
  }

  /** Build a `NetProxy`` from the given `SocketChannel` */
  def newChannel(theChannel: SocketChannel): TypedTCPChannel[OUT,IN] = new TypedTCPChannel[OUT,IN] with Mixin {
    val channel = theChannel
    val output = new DataOutputStream(new BufferedOutputStream(java.nio.channels.Channels.newOutputStream(channel)))
    val input = new DataInputStream(new BufferedInputStream(java.nio.channels.Channels.newInputStream(channel)))
  }

  /**
    * Build a `NetProxy`` from the given `Socket`. Intended for use only for SSL/TLS Sockets.
    */
  def newChannel(theSocket: Socket): TypedSSLChannel[OUT,IN] = new TypedSSLChannel[OUT,IN] with Mixin {
    val socket = theSocket
    val output = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream))
    val input  = new DataInputStream(new BufferedInputStream(socket.getInputStream))
  }

  /**
    * Build a `Codec` from the given `input` and `output` streams.
    */
  def newCodec(_output: OutputStream, _input: InputStream): Codec[OUT, IN] = new Codec[OUT,IN]  with Mixin {
    val output = new DataOutputStream(new BufferedOutputStream(_output))
    val input = new DataInputStream(new BufferedInputStream(_input))
  }
}
