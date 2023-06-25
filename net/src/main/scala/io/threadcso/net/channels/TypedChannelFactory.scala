package io.threadcso.net.channels

import io.threadcso.net.codec.Codec

import java.io.{InputStream, OutputStream}
import java.nio.channels.SocketChannel

/**
  *  A `TypedChannelFactory`s are used to associate various forms of wire-encoded byte-streaming input and output with
  *  typed channels capable of encoding and decoding byte-streams to and from `OUT` and `IN`. Although it is usually
  *  sufficient to define only the `newCodec` method, it may be more efficient for certain encodings, or require
  *  less new code to be written, to define the two `newChannel` methods independently.
  *
  *  For examples of implementations
  *
  * @see io.threadcso.net.factory.StringChannelCRLF
  * @see io.threadcso.net.factory.StreamerChannel
  *
  */
trait TypedChannelFactory[-OUT, +IN] {

  /** Build a `TypedTCPChannel` from the given SocketChannel` */
  def newChannel(channel: SocketChannel): TypedTCPChannel[OUT, IN]

  /** Build a `TypedSSLChannel` from the given `Socket`.
    * Normally used only for SSL/TLS Sockets.
    */
  def newChannel(socket: java.net.Socket): TypedSSLChannel[OUT, IN]

  /**
    * Build a (typed) `Codec` from the given `input` and `output` streams.
    */
  def newCodec(output: OutputStream, input: InputStream): Codec[OUT, IN]
}
