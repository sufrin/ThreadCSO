package ox.net

import ox.net.codec.Codec

import java.io.{InputStream, OutputStream}
import java.nio.channels.SocketChannel

trait TypedChannelFactory[-OUT, +IN] {

  /** Build a `NetProxy`` from the given `SocketChannel` */
  def newChannel(channel: SocketChannel): TypedTCPChannel[OUT, IN]

  /** Build a `NetProxy`` from the given `Socket`
    * Expected to be used only for SSL/TLS Sockets.
    */
  def newChannel(socket: java.net.Socket): TypedSSLChannel[OUT, IN]

  /** Build a `Codec` from the given `input` and `output` streams.
    */
  def newCodec(output: OutputStream, input: InputStream): Codec[OUT, IN]
}
