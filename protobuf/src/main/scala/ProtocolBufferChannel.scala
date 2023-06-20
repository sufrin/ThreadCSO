package io.threadcso.net.factory

import io.threadcso._
import io.threadcso.net._
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import io.threadcso.net.channels.{TypedChannelFactory, TypedSSLChannel, TypedTCPChannel}
import io.threadcso.net.codec.{Codec, EndOfInputStream}

import java.io._
import java.net.Socket
import java.nio.channels.SocketChannel

/**
 *  Protocol Buffer Channel Factory. Wire-encodings are `[length; protobufencoding]`, where
 *  length is expressed as a 4-byte LITTLE-ENDIAN integer.
 *
 *  The `companion` parameter is used for input: it must be the `GeneratedMessageCompanion[IN]`. This can
 *  easily be done (apologies for the boilerplate). For example, if `Person` is a message type defined
 *  by a protocol buffer specification, a channel factory for inputting and outputting `Persons` is
 *  constructed by:
 *{{{
 *  val Factory = new io.threadcso.net.factory.ProtocolBufferChannel[Person,Person](Person)
 *}}}
 *  
 */

class ProtocolBufferChannel[OUT <: GeneratedMessage, IN <: GeneratedMessage](companion: GeneratedMessageCompanion[IN])
      extends TypedChannelFactory[OUT,IN] {

  def newChannel(theChannel: SocketChannel): TypedTCPChannel[OUT,IN] = new TypedTCPChannel[OUT, IN] with Mixin {
    val channel = theChannel
    val output = new BufferedOutputStream(java.nio.channels.Channels.newOutputStream(channel))
    val input = new BufferedInputStream(java.nio.channels.Channels.newInputStream(channel))
  }

  /**
    * Build a `NetProxy`` from the given `Socket`
    * Expected to be used only for SSL/TLS Sockets.
    */
  def newChannel(theSocket: Socket): TypedSSLChannel[OUT, IN] = new TypedSSLChannel[OUT, IN] with Mixin {
    val socket = theSocket
    val output = new BufferedOutputStream(socket.getOutputStream)
    val input  = new BufferedInputStream(socket.getInputStream)
  }

  /**
    * Build a `Codec` from the given `input` and `output` streams.
    */
  def newCodec(_output: OutputStream, _input: InputStream): Codec[OUT, IN] = new Codec[OUT,IN] with Mixin {
    val output = new BufferedOutputStream(_output)
    val input = new BufferedInputStream(_input)
  }

  trait Mixin {
    val input:  InputStream
    val output: OutputStream
    var inOpen, outOpen = true
    def sync:   Boolean

    def decode(): IN = try {
        @inline def b(): Byte = (input.read()&0xff).toByte
        val b0 = b()
        val b1 = b() << 8
        val b2 = b() << 16
        val b3 = b() << 24
        val length = b0 | b1 | b2 | b3
        if (length<0) throw throw new EndOfInputStream(input)
        val bytes = new Array[Byte](length)
        input.read(bytes)
        val obj = companion.parseFrom(bytes): IN
        obj
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
    def encode(value: OUT): Unit = {
      val bytes  = value.toByteArray
      var length = bytes.size
      println(s"length=$length")
      for { i<-0 until 4 } { output.write((length & 0xff).toByte); length = length >> 8 }
      for { i<-0 until bytes.size} output.write(bytes(i))
      output.flush()
    }

    /**
      * Stop encoding and release/close any engaged resources,
      * including the associated stream.
      */
    def closeOut(): Unit = output.close()
  }
}

