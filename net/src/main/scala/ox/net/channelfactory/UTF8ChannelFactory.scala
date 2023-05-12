package ox.net.channelfactory

import ox.net.channelfactory.CRLFChannelFactory.{CRLFMixin, UTF8}
import ox.net.codec.Codec
import ox.net.{ChannelOptions, TypedChannelFactory, TypedSSLChannel, TypedTCPChannel}

import java.io._
import java.net.Socket
import java.nio.channels.SocketChannel

object UTF8ChannelFactory extends TypedChannelFactory[String,String] {

  trait UTF8Mixin {
    val input: InputStream
    val output: OutputStream

    def sync: Boolean
    def closeOut(): Unit = output.close()
    def closeIn(): Unit = input.close()

    // Laziness here is on account of a cryptointeraction with DataXputStream
    // Non-lazy declaration fails at the first read/write of the DataXputStream
    // Exception is reported (correctlty) by Scala as a null value for input/output
    // TODO: Investigate why this works. Likely connected with mixin implementation detail?
    lazy val out = new java.io.DataOutputStream(output)
    lazy val in  = new java.io.DataInputStream(input)
    var outOpen, inOpen: Boolean = true

    def canEncode: Boolean = outOpen

    def encode(value: String): Unit = try {
      out.writeUTF(value)
      if (sync) out.flush()
    } catch {
      case exn: IOException =>
        outOpen = false
    }

    def canDecode: Boolean = inOpen

    def decode(): String = try in.readUTF() catch {
      case exn: EOFException =>
        inOpen = false
        ""
    }
  }

  def newChannel(theChannel: SocketChannel): TypedTCPChannel[String, String] = new TypedTCPChannel[String, String] with UTF8Mixin {
    val channel: SocketChannel = theChannel
    val input:   InputStream   = java.nio.channels.Channels.newInputStream(channel)
    val output:  OutputStream  = java.nio.channels.Channels.newOutputStream(channel)
  }

  def newChannel(theSocket: Socket): TypedSSLChannel[String, String] = new TypedSSLChannel[String, String] with UTF8Mixin {
    val socket: Socket = theSocket
    val input: InputStream = theSocket.getInputStream
    val output: OutputStream = theSocket.getOutputStream
  }

  def newCodec(_output: OutputStream, _input: InputStream): Codec[String, String] =
    new Codec[String, String] with CRLFMixin {
      val output = new BufferedWriter(new OutputStreamWriter(_output, UTF8.newEncoder()), ChannelOptions.outSize)
      val input = new BufferedReader(new InputStreamReader(_input, UTF8.newDecoder()), ChannelOptions.inSize) // TODO: factory parameters
    }

}
