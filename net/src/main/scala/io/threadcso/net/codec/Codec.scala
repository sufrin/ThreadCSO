package io.threadcso.net.codec

import io.threadcso.process.Stopped

class EndOfInputStream(stream: Any) extends Stopped
class EndOfOutputStream(stream: Any) extends Stopped

/**
  * An `Encoder[O]` is implicitly associated with a network output stream,
  * to which it forms a bridge: transforming values of type `O`
  * to a network representation in the form of (a stream of) bytes.
  *
  * @tparam O the type of data to be output by the encoder
  */
trait Encoder[-O] {
  private var _sync: Boolean = true
  /**
    * Encode `output` and transmit its representation to
    * the associated network stream
    */
  def encode(output: O): Unit

  /** The most recent `toStream` was successful if true; else the associated stream closed or the toStream failed */

  /** When `sync` is true of an encoder, its associated stream must be flushed after an `toStream`. */
  def sync: Boolean = _sync

  /**
    * Set sync.
    */
  def sync_=(sync: Boolean): Unit = _sync = sync

  /**
    * Stop encoding and release/close any engaged resources,
    * including the associated stream.
    */
  def closeOut(): Unit

  var lastEncoderException: Option[Throwable] = None
}

/**
  * A `Decoder[O]` is implicitly associated with a network input stream, to which
  * it forms a bridge: transforming a network representation in the form of
  * (a stream of bytes) into values of type `I`.
  *
  * @tparam I the type of data to be input
  */
trait Decoder[+I] {
  /**
    * Decode the next encoded item on the associated network stream
    */
  def decode(): I

  /**
    * The most recent `fromStream` yielded a valid result if true;
    * else the associated stream closed or the fromStream failed.
    */
  def canDecode: Boolean

  /**
    * Stop decoding and release/close any engaged resources,
    * including the associated stream.
    */
  def closeIn(): Unit

  var lastDecoderException: Option[Throwable] = None
}

/**
  * A related Encoder/Decoder pair. Usually, though not always,
  * `O` and `I` are the same type. When this is true, the
  * compatibility of the decoder with the encoder can be tested by
  * connecting its associated input and output streams suitably.
  *
  * @tparam O the type of data to be output by the encoder.
  * @tparam I the type of data to be input.
  */
trait Codec[-O,+I] extends Encoder[O] with Decoder[I] { }