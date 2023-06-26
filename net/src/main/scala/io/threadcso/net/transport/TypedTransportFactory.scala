package io.threadcso.net.transport

import io.threadcso.net.codec.Codec

import java.io.{InputStream, OutputStream}
import java.nio.channels.SocketChannel

/**
  *  `TypedTransportFactory`s are used to associate various forms of wire-encoded (byte-streaming) input and output with
  *  typed transport capable of encoding and decoding byte-streams to and from `OUT` and `IN`. Although it is usually
  *  sufficient to define only the `newCodec` method, it may be more efficient for certain encodings, or require
  *  less new code to be written, to define the two `newTransport` methods independently.
  *
  *  For examples of implementations
  *
  * @see io.threadcso.net.factory.StringTransportCRLF
  * @see io.threadcso.net.factory.StreamerTransport
  *
  */
trait TypedTransportFactory[-OUT, +IN] {

  /** Build a `TypedTCPTransport` from the given SocketChannel` */
  def newTransport(channel: SocketChannel): TypedTCPTransport[OUT, IN]

  /** Build a `TypedSSLTransport` from the given `Socket`.
    * Normally used only for SSL/TLS Sockets.
    */
  def newTransport(socket: java.net.Socket): TypedSSLTransport[OUT, IN]

  /**
    * Build a (typed) `Codec` from the given `input` and `output` streams.
    */
  def newCodec(output: OutputStream, input: InputStream): Codec[OUT, IN]
}
