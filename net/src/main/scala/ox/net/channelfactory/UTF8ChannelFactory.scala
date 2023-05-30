package ox.net.channelfactory

import ox.net.codec.{Codec, EndOfOutputStream}
import ox.net.{ChannelOptions, TypedChannelFactory, TypedSSLChannel, TypedTCPChannel}

import java.io._
import java.net.Socket
import java.nio.channels.SocketChannel

object UTF8ChannelFactory extends ox.logging.Log("UTF8ChannelFactory") with TypedChannelFactory[String,String] {
  override def toString = "UTF8 Factory"

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
      if (logging) finest(s"UTF: wrote #${value.length}")
    } catch {
      case exn: IOException =>
        outOpen = false
        if (logging) finest(s"UTF: Encode IOException $exn")
        throw new EndOfOutputStream(out)
    }


    def decode(): String = try {
      val r = in.readUTF()
      if (logging) finest(s"UTF: Decode #${r.length} (${r.subSequence(0, 40 min r.length)}...")
      r
    } catch { // exceptions caused by datagram truncation
      case exn: EOFException =>
        inOpen = false
        fine(s"UTF: Decode EOF $exn")
        throw exn
      case exn: UTFDataFormatException =>
        inOpen = false
        warning(s"UTF: Decode error $exn")
        throw exn
    }
  }

  def newChannel(theChannel: SocketChannel): TypedTCPChannel[String, String] = new TypedTCPChannel[String, String] with Mixin {
    val channel: SocketChannel = theChannel
    val input:   InputStream   = java.nio.channels.Channels.newInputStream(channel)
    val output:  OutputStream  = java.nio.channels.Channels.newOutputStream(channel)
  }

  def newChannel(theSocket: Socket): TypedSSLChannel[String, String] = new TypedSSLChannel[String, String] with Mixin {
    val socket: Socket = theSocket
    val input: InputStream = theSocket.getInputStream
    val output: OutputStream = theSocket.getOutputStream
  }

  def newCodec(_output: OutputStream, _input: InputStream): Codec[String, String] =
    new Codec[String, String] with Mixin {
      val output = new BufferedOutputStream(_output, ChannelOptions.outSize)
      val input  = new BufferedInputStream(_input, ChannelOptions.inSize)
    }
}
