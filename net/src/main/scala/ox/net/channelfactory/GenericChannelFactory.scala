package ox.net.channelfactory

import org.velvia.msgpack.{Codec => ImplicitCodec}
import ox.net.TypedChannelFactory
import ox.net.TypedSSLChannel
import ox.net.TypedTCPChannel

import java.io._
import java.net.Socket
import java.nio.channels.SocketChannel



/**
  * A class which can be specialized as a builder of type-specific channels for serializable types for which (implicit)
  * `org.velvia.msgpack.Codec`s can be synthesized or directly constructed. Although it can be a little tedious to
  * synthesize the `Codec`s the language machinery is just about acceptable. Cases in point are the cases in
  * `MessagePackTests` in which `Ty` is defined respectively by:
  * {{{
  *   type Ty = (Int, Int)
  *   type Ty = (Int, String)
  *   case class Ty(times: Int, string: String)
  * }}}
  *
  * The derivations are (respectively)
  * {{{
  *    implicit object TyCodec extends TupleCodec2[Int,Int]
  *    implicit object TyCodec extends TupleCodec2[Int, String]
  *    implicit object TyCodec extends
  *       CaseClassCodec2[Ty, Int, String]
  *       ( Ty, { case Ty(t, s) => Some(t, s)})
  * }}}
  *
  * Though I'd have preferred to copy-and-paste
  * {{{
  *   implicit object TyCodec extends Codec[Ty]
  * }}}
  */

class GenericChannelFactory[T : ImplicitCodec] extends ox.logging.Log("GenericChannelFactory") with TypedChannelFactory[T, T] {
  def newChannel(theChannel: SocketChannel): TypedTCPChannel[T,T] = new TypedTCPChannel[T,T] with Mixin {
    val channel = theChannel
    val output = new DataOutputStream(new BufferedOutputStream(java.nio.channels.Channels.newOutputStream(channel)))
    val input = new DataInputStream(new BufferedInputStream(java.nio.channels.Channels.newInputStream(channel)))
  }

  /**
    * Build a `NetProxy`` from the given `Socket`
    * Expected to be used only for SSL/TLS Sockets.
    */
  def newChannel(theSocket: Socket): TypedSSLChannel[T,T] = new TypedSSLChannel[T,T] with Mixin {
    val socket = theSocket
    val output = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream))
    val input = new DataInputStream(new BufferedInputStream(socket.getInputStream))
  }

  /**
    * Build a `Codec` from the given `input` and `output` streams.
    */
  def newCodec(_output: OutputStream, _input: InputStream): ox.net.codec.Codec[T,T] = new ox.net.codec.Codec[T,T] with Mixin {
    val output = new DataOutputStream(new BufferedOutputStream(_output))
    val input = new DataInputStream(new BufferedInputStream(_input))
  }

  trait Mixin {
    val input: DataInputStream
    val output: DataOutputStream
    var inOpen, outOpen = true
    def sync: Boolean

    def decode(): T = try {
      org.velvia.MessagePack.unpack(input)
    } catch {
      case exn: UTFDataFormatException => inOpen = false; throw new ox.net.codec.EndOfInputStream(input)
      case exn: EOFException => inOpen = false; throw new ox.net.codec.EndOfInputStream(input)
      case exn: IOException => inOpen = false; throw new ox.net.codec.EndOfInputStream(input)
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
    def encode(value: T): Unit = {
      org.velvia.MessagePack.pack(value, output)
      if (sync) output.flush()
    }

    /**
      * Stop encoding and release/close any engaged resources,
      * including the associated stream.
      */
    def closeOut(): Unit = output.close()
  }
}

