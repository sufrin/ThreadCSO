package io.threadcso.net.factory

import io.threadcso.net.channels.{Options, TypedChannelFactory, TypedSSLChannel, TypedTCPChannel}
import io.threadcso.net.codec.Codec

import java.io._
import java.net.Socket
import java.nio.channels.SocketChannel
import javax.net.ssl.SSLSocket

/**
  *  A channel factory that defines a byte-stream encoding for
  *  strings suitable for use in the `http` protocol.
  *  Byte stream representations are terminated by the byte
  *  doublet `"\r\n"`.
  */
object StringChannelCRLF extends TypedChannelFactory[String,String] {
  val log = new ox.logging.Log()

  override def toString = "CRLF Factory"

  trait Mixin {
    val output: java.io.Writer
    val input:  java.io.Reader
    def sync:   Boolean

    def closeOut(): Unit = output.close()
    def closeIn():  Unit = input.close()

    var inOpen, outOpen: Boolean = true
    def canEncode: Boolean = outOpen
    def canDecode: Boolean = inOpen

    def decode(): String = {
        val result            = new StringBuffer()
        var cr:      Boolean  = false
        var reading: Boolean  = true
        try {
          while (reading) {
            val c = input.read()
            c match {
              case -1 =>
                if (log.logging) log.warning(s"crlf: fromStream: stream ended before string")
                throw new EOFException()
              case '\r' =>
                cr = true
              case '\n' =>
                if (cr) reading = false else result.append(c.toChar)
              case _ =>
                cr = false
                result.append(c.toChar)
            }
          }
          result.toString
        }
        catch {
          // a socket closed in flight
          case exn: java.net.SocketException => throw new EOFException()
        }
    }

    def encode(value: String): Unit = {
      output.write(value)
      output.write("\r\n")
      if (sync) output.flush()
    }
  }

  val UTF8 = java.nio.charset.Charset.forName("UTF-8")

  def newChannel(theChannel: SocketChannel): TypedTCPChannel[String, String] = new TypedTCPChannel[String, String] with Mixin {
    val channel: SocketChannel = theChannel
    val output = java.nio.channels.Channels.newWriter(channel, UTF8.newEncoder(), Options.outBufSize)
    val input  = java.nio.channels.Channels.newReader(channel, UTF8.newDecoder(), Options.inBufSize)
    sync = true
  }

  def newChannel(theSocket: Socket): TypedSSLChannel[String, String] = new TypedSSLChannel[String, String] with Mixin {
    val socket: Socket = theSocket
    val output = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream, UTF8.newEncoder()), Options.outBufSize)
    val input = new InputStreamReader(socket.getInputStream, UTF8.newDecoder())
    sync = true

    override def closeOut(): Unit = {
      super.close()
      socket match {
        case ssl: SSLSocket =>
          if (log.logging) log.fine(s"SSL socket $ssl shutting down output")
          ssl.shutdownOutput()
      }
    }

    override def closeIn(): Unit = {
      super.close()
      socket match {
        case ssl: SSLSocket =>
          if (log.logging) log.fine(s"SSL socket $ssl shutting down input")
          ssl.shutdownInput()
      }
    }
  }

  def newCodec(_output: OutputStream, _input: InputStream): Codec[String,String] =
      new Codec[String,String] with Mixin {
        val output = new BufferedWriter(new OutputStreamWriter(_output, UTF8.newEncoder()), Options.outBufSize)
        val input  = new InputStreamReader(_input, UTF8.newDecoder()) // TODO: factory parameters
        sync = true
      }
}
