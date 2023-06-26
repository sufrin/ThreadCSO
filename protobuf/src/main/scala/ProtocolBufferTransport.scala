package io.threadcso.net.factory

import io.threadcso._
import io.threadcso.net._
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import io.threadcso.net.transport.{TypedTransport,TypedTransportFactory, TypedSSLTransport, TypedTCPTransport}
import io.threadcso.net.codec.{Codec, EndOfInputStream}

import java.io._
import java.net.Socket
import java.nio.channels.SocketChannel

/**
 *  Protocol Buffer Transport Factory. Wire-encodings are `[length; protobufencoding]`, where
 *  length is expressed as a 4-byte bigendian integer (as defined by java `Data{In/Put}put` streams).
 *
 *  The `companion` parameter is used for input: it must be the `GeneratedMessageCompanion[IN]`. This can
 *  easily be done (apologies for the boilerplate). For example, if `Person` is a message type defined
 *  by a protocol buffer specification, a channel factory for inputting and outputting `Persons` is
 *  constructed by:
 *{{{
 *  val Factory = new io.threadcso.net.factory.ProtocolBufferTransport[Person,Person](Person)
 *}}}
 *
 */

class ProtocolBufferTransport[OUT <: GeneratedMessage, IN <: GeneratedMessage](companion: GeneratedMessageCompanion[IN])
      extends TypedTransportFactory[OUT,IN] {

  def newTransport(theChannel: SocketChannel): TypedTCPTransport[OUT,IN] = new TypedTCPTransport[OUT, IN] with Mixin {
    val channel = theChannel
    val output = new DataOutputStream(java.nio.channels.Channels.newOutputStream(channel))
    val input = new DataInputStream(java.nio.channels.Channels.newInputStream(channel))
  }

  /**
    * Build a `TypedTransport` from the given `Socket`
    * Expected to be used only for SSL/TLS Sockets.
    */
  def newTransport(theSocket: Socket): TypedSSLTransport[OUT, IN] = new TypedSSLTransport[OUT, IN] with Mixin {
    val socket = theSocket
    val output = new DataOutputStream(socket.getOutputStream)
    val input  = new DataInputStream(socket.getInputStream)
  }

  /**
    * Build a `Codec` from the given `input` and `output` streams.
    */
  def newCodec(_output: OutputStream, _input: InputStream): Codec[OUT, IN] = new Codec[OUT,IN] with Mixin {
    val output = new DataOutputStream(_output)
    val input = new DataInputStream(_input)
  }


  trait Mixin {
    val input:  DataInputStream
    val output: DataOutputStream
    var inOpen, outOpen = true
    def sync:   Boolean


    def decode(): IN = try {
        val length = input.readInt()
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
      output.writeInt(length)
      output.write(bytes)
      output.flush()
    }

    /**
      * Stop encoding and release/close any engaged resources,
      * including the associated stream.
      */
    def closeOut(): Unit = output.close()
  }
}

