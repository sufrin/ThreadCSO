package io.threadcso.net.factory

import io.threadcso.net.transport.{Options, TypedTransportFactory, TypedSSLTransport, TypedTCPTransport}
import io.threadcso.net.codec.{Codec, EndOfOutputStream}

import java.io._
import java.net.Socket
import java.nio.channels.SocketChannel

/**
  *  A channel factory that provides the byte-stream encoding for
  *  strings implemented by `java.io.DataOutputStream.writeUTF`
  *  and `java.io.DataInputStream.readUTF`
  */
object StringTransportUTF8 extends TypedTransportFactory[String,String] {
  val log = ox.logging.Log("StringTransportUTF8")

  override def toString = "UTF8 Channel Factory"

  trait Mixin {
    val input: InputStream
    val output: OutputStream

    def sync: Boolean
    def closeOut(): Unit = output.close()
    def closeIn():  Unit = input.close()

    lazy val out = new java.io.DataOutputStream(output)
    lazy val in  = new java.io.DataInputStream(input)

    var outOpen, inOpen: Boolean = true
    def canEncode: Boolean = outOpen
    def canDecode: Boolean = inOpen

    def encode(value: String): Unit = try {
      out.writeUTF(value)
      if (sync) out.flush()
      if (log.logging) log.finest(s"UTF: wrote #${value.length}")
    } catch {
      case exn: IOException =>
        outOpen = false
        if (log.logging) log.finest(s"UTF: Encode IOException $exn")
        throw new EndOfOutputStream(out)
    }

    def decode(): String = try {
      val r = in.readUTF()
      if (log.logging) log.finest(s"UTF: Decode #${r.length} (${r.subSequence(0, 40 min r.length)}...")
      r
    } catch { // exceptions caused by datagram truncation
      case exn: EOFException =>
        inOpen = false
        if (log.logging) log.fine(s"UTF: Decode EOF $exn")
        throw exn
      case exn: UTFDataFormatException =>
        inOpen = false
        log.severe(s"UTF: Decode error $exn")
        throw exn
    }
  }

  def newTransport(theChannel: SocketChannel): TypedTCPTransport[String, String] = new TypedTCPTransport[String, String] with Mixin {
    val channel: SocketChannel = theChannel
    val input:   InputStream   = java.nio.channels.Channels.newInputStream(channel)
    val output:  OutputStream  = java.nio.channels.Channels.newOutputStream(channel)
  }

  def newTransport(theSocket: Socket): TypedSSLTransport[String, String] = new TypedSSLTransport[String, String] with Mixin {
    val socket: Socket = theSocket
    val input: InputStream = theSocket.getInputStream
    val output: OutputStream = theSocket.getOutputStream
  }

  def newCodec(_output: OutputStream, _input: InputStream): Codec[String, String] =
    new Codec[String, String] with Mixin {
      val output = new BufferedOutputStream(_output, Options.outBufSize)
      val input  = new BufferedInputStream(_input, Options.inBufSize)
    }
}
