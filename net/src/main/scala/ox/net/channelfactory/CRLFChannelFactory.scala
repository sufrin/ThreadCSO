package ox.net.channelfactory

import ox.net.codec.Codec
import ox.net.{ChannelOptions, TypedChannelFactory, TypedSSLChannel, TypedTCPChannel}

import java.io._
import java.net.Socket
import java.nio.channels.SocketChannel
import javax.net.ssl.SSLSocket

object CRLFChannelFactory extends TypedChannelFactory[String,String] {
  val crlf = ox.logging.Log("crlf")

  trait CRLFMixin {
    val output: java.io.Writer
    val input:  java.io.Reader
    def sync:   Boolean

    def closeOut(): Unit = output.close()
    def closeIn(): Unit = input.close()

    var inOpen, outOpen: Boolean = true

    def canEncode: Boolean = outOpen
    def canDecode: Boolean = inOpen

    def decode(): String = {
        val result       = new StringBuffer()
        var cr: Boolean  = false
        var going: Boolean = true
        while (going) {
          val c = try { input.read() } catch {
            case exn: Throwable =>
              crlf.finest(s"Decode: empty input $exn")
              crlf.finest(exn)
              -2
          }
          c match {
            case -2 =>
              going = false
              inOpen = false
            case -1 =>
              crlf.finest(s"crlf: decode: channel closed")
              going = false
              inOpen = false
            case '\r' =>
              cr = true
            case '\n' =>
              if (cr) going=false else result.append(c.toChar)
            case _ =>
              cr = false
              result.append(c.toChar)
          }
        }
        if (inOpen) result.toString else ""
    }

    def encode(value: String): Unit = {
      output.write(value)
      output.write("\r\n")
      if (sync) output.flush()
    }
  }

  val UTF8 = java.nio.charset.Charset.forName("UTF-8")

  def newChannel(theChannel: SocketChannel): TypedTCPChannel[String, String] = new TypedTCPChannel[String, String] with CRLFMixin {
    val channel: SocketChannel = theChannel
    val output = java.nio.channels.Channels.newWriter(channel, UTF8.newEncoder(), ChannelOptions.outSize)  // TODO: factory parameters
    val input  = java.nio.channels.Channels.newReader(channel, UTF8.newDecoder(), ChannelOptions.inSize)   // TODO: factory parameters
    sync = true
  }

  def newChannel(theSocket: Socket): TypedSSLChannel[String, String] = new TypedSSLChannel[String, String] with CRLFMixin {
    val socket: Socket = theSocket
    val output = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream, UTF8.newEncoder()), ChannelOptions.outSize) // TODO: factory parameters
    val input = new InputStreamReader(socket.getInputStream, UTF8.newDecoder()) // TODO: factory parameters
    sync = true

    override def closeOut(): Unit = {
      super.close()
      socket match {
        case ssl: SSLSocket =>
          log.finest(s"SSL socket $ssl shutting down output")
          ssl.shutdownOutput()
      }
    }

    override def closeIn(): Unit = {
      super.close()
      socket match {
        case ssl: SSLSocket =>
          log.finest(s"SSL socket $ssl shutting down input")
          ssl.shutdownInput()
      }
    }
  }

  def newCodec(_output: OutputStream, _input: InputStream): Codec[String,String] =
      new Codec[String,String] with CRLFMixin {
        val output = new BufferedWriter(new OutputStreamWriter(_output, UTF8.newEncoder()), ChannelOptions.outSize)
        val input = new InputStreamReader(_input, UTF8.newDecoder()) // TODO: factory parameters
        sync = true
      }
}
