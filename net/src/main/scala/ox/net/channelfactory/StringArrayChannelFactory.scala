package ox.net.channelfactory

import ox.net.codec.{Codec, EndOfInputStream}
import ox.net.{TypedChannelFactory, TypedSSLChannel, TypedTCPChannel}

import java.io._
import java.net.Socket
import java.nio.channels.SocketChannel

/**
  * Simple model of a ChannelFactory for a composite type.
  * Provided the type can be streamed to an `DataOutputStream`, it's straightforward
  * to provide the corresponding `ChannelFactory`
  */
object StringArrayChannelFactory extends ox.logging.Log("StringArrayChannelFactory") with TypedChannelFactory[Array[String],Array[String]] {
  type StringArray = Array[String]

  trait Mixin {
    val input:  DataInputStream
    val output: DataOutputStream
    var inOpen, outOpen = true

    /**
      * Decode the next encoded item on the associated network stream
      */
    def decode(): StringArray = try {
      val count = input.readInt
      val result = new Array[String](count)
      for {i <- 0 until count} result(i) = input.readUTF()
      result
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
    def encode(array: StringArray): Unit = {
      val count = array.size
      output.writeInt(count)
      for {elt <- array} output.writeUTF(elt)
      output.flush()
    }

    /**
      * Stop encoding and release/close any engaged resources,
      * including the associated stream.
      */
    def closeOut(): Unit = output.close()
  }
  /** Build a `NetProxy`` from the given `SocketChannel` */
  def newChannel(theChannel: SocketChannel): TypedTCPChannel[StringArray,StringArray] = new TypedTCPChannel[StringArray,StringArray] with Mixin {
    val channel = theChannel
    val output = new DataOutputStream(new BufferedOutputStream(java.nio.channels.Channels.newOutputStream(channel)))
    val input = new DataInputStream(new BufferedInputStream(java.nio.channels.Channels.newInputStream(channel)))
  }

  /**
    * Build a `NetProxy`` from the given `Socket`
    * Expected to be used only for SSL/TLS Sockets.
    */
  def newChannel(theSocket: Socket): TypedSSLChannel[StringArray,StringArray] = new TypedSSLChannel[StringArray, StringArray] with Mixin {
    val socket = theSocket
    val output = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream))
    val input = new DataInputStream(new BufferedInputStream(socket.getInputStream))
  }

  /**
    * Build a `Codec` from the given `input` and `output` streams.
    */
  def newCodec(_output: OutputStream, _input: InputStream): Codec[StringArray, StringArray] = new Codec[StringArray,StringArray] with Mixin {
    val output = new DataOutputStream(new BufferedOutputStream(_output))
    val input = new DataInputStream(new BufferedInputStream(_input))
  }
}
