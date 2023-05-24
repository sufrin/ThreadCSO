package ox.net.channelfactory

import ox.net.codec.{Codec, EndOfInputStream, DataStreamEncoding}
import ox.net.{TypedChannelFactory, TypedSSLChannel, TypedTCPChannel}

import java.io._
import java.net.Socket
import java.nio.channels.SocketChannel

/**
  * An abstract `ChannelFactory` for a type that has a `DataStreamEncoding` that can be synthesised or
  * inferred (for example by using the tools of `DataStreamEncoding`)
  *
  * For efficiency we also provide `VelviaChannelFactory`, for direct use with encodings made
  * from encodings originating with `org.velvia.MessagePack`. These encodings are in general
  * very much more concise than those originating with `DataStreamEncoding`. They have the
  * added (killer) virtue of having many language bindings to them and facilitating interworking.
    *
  */
class DataStreamChannelFactory[T](implicit encoding: DataStreamEncoding.Stream[T])
       extends ox.logging.Log("DataStreamChannelFactory")
       with TypedChannelFactory[T,T] {

  /**
    * This mixin requires `input` and `output` datastreams, and a `sync` in its
    * host class.
    */
  trait Mixin {
    val input:  DataInputStream
    val output: DataOutputStream
    var inOpen, outOpen = true
    def sync: Boolean

    /**
      * Decode the next encoded item on the associated network stream
      */
    def decode(): T = try {
      encoding.decode(input)
    } catch {
      case exn: UTFDataFormatException => inOpen = false; throw new EndOfInputStream(input)
      case exn: EOFException => inOpen = false; throw new EndOfInputStream(input)
      case exn: IOException => inOpen = false; throw new EndOfInputStream(input)
    }

    /**
      * The most recent `decode` yielded a valid result if true;
      * else the associated stream closed or the decode failed.
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
    def encode(t: T): Unit = {
      encoding.encode(output, t)
      if (sync) output.flush()
    }

    /**
      * Stop encoding and release/close any engaged resources,
      * including the associated stream.
      */
    def closeOut(): Unit = output.close()
  }

  /** Build a `NetProxy`` from the given `SocketChannel` */
  def newChannel(theChannel: SocketChannel): TypedTCPChannel[T,T] = new TypedTCPChannel[T,T] with Mixin {
    val channel = theChannel
    val output = new DataOutputStream(new BufferedOutputStream(java.nio.channels.Channels.newOutputStream(channel)))
    val input = new DataInputStream(new BufferedInputStream(java.nio.channels.Channels.newInputStream(channel)))
  }

  /**
    * Build a `NetProxy`` from the given `Socket`
    * Expected to be used only for SSL/TLS Sockets.
    */
  def newChannel(theSocket: Socket): TypedSSLChannel[T,T] = new TypedSSLChannel[T, T] with Mixin {
    val socket = theSocket
    val output = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream))
    val input  = new DataInputStream(new BufferedInputStream(socket.getInputStream))
  }

  /**
    * Build a `Codec` from the given `input` and `output` streams.
    */
  def newCodec(_output: OutputStream, _input: InputStream): Codec[T, T] = new Codec[T,T]  with Mixin {
    val output = new DataOutputStream(new BufferedOutputStream(_output))
    val input = new DataInputStream(new BufferedInputStream(_input))
  }
}
