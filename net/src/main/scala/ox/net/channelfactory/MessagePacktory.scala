package ox.net.channelfactory

import ox.net.{TypedChannelFactory, TypedSSLChannel, TypedTCPChannel}
import ox.net.codec.{Codec, EndOfInputStream}

import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream, EOFException, IOException, InputStream, OutputStream, UTFDataFormatException}
import java.net.Socket
import java.nio.channels.SocketChannel 

object MessagePacktory extends ox.logging.Log("MessagePacktory") with TypedChannelFactory[Any,Any] {
  def newChannel(theChannel: SocketChannel): TypedTCPChannel[Any, Any] = new TypedTCPChannel[Any, Any] with Mixin {
    val channel = theChannel
    val output = new DataOutputStream(new BufferedOutputStream(java.nio.channels.Channels.newOutputStream(channel)))
    val input = new DataInputStream(new BufferedInputStream(java.nio.channels.Channels.newInputStream(channel)))
  }

  /**
    * Build a `NetProxy`` from the given `Socket`
    * Expected to be used only for SSL/TLS Sockets.
    */
  def newChannel(theSocket: Socket): TypedSSLChannel[Any, Any] = new TypedSSLChannel[Any, Any] with Mixin {
    val socket = theSocket
    val output = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream))
    val input = new DataInputStream(new BufferedInputStream(socket.getInputStream))
  }

  /**
    * Build a `Codec` from the given `input` and `output` streams.
    */
  def newCodec(_output: OutputStream, _input: InputStream): Codec[Any, Any] = new Codec[Any, Any] with Mixin {
    val output = new DataOutputStream(new BufferedOutputStream(_output))
    val input = new DataInputStream(new BufferedInputStream(_input))
  }

  trait Mixin {
    val input: DataInputStream
    val output: DataOutputStream
    var inOpen, outOpen = true
    def sync: Boolean


    def decode(): Any = try {

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
    def encode(value: Any): Unit = {
      if (sync) output.flush()
    }

    /**
      * Stop encoding and release/close any engaged resources,
      * including the associated stream.
      */
    def closeOut(): Unit = output.close()
  }
}

