package io.threadcso.net.transport

import io.threadcso.net.codec.{Decoder, Encoder}
import io.threadcso.process.PROC
import io.threadcso.{!!, ??, proc, repeat}

object TypedTransport {
  val log = ox.logging.Logging.Log()
}

/**
  *  A mixin trait that is implicitly associated with a pair of network byte-streams defined by the
  *  classes into which it is mixed.
  *
  *  Defines daemon processes to encode and copy values to the network  and to decode
  *  values from the network.
  *
  */
trait TypedTransport[-OUT,+IN] extends TypedOutputTransport[OUT] with TypedInputTransport[IN] {
  override def toString: String = "TypedTransport"
}

/**
  *  Provides a daemon process `transportToNet` to transport values to the
  *  network in their byte-stream encoded form.
  * @tparam O the type of data to be output by the encoder
  */
trait TypedOutputTransport[-O] extends Encoder[O] {
  private val log     = TypedTransport.log
  @inline private def logging = log.logging

  /**
    * Returns a daemon process that, when running, repeatedly reads values from `in` and sends
    * their byte-stream forms to the network.
    */
  def transportToNet(in: ??[O]): PROC = proc(this.toString + ".transportToNet") {
    try {
      repeat {
        val value = in ? ()
        if (logging) log.finest(s"Copy to netchannels toStream($value)")
        encode(value)
        if (logging) log.finest(s"Copy to netchannels encoded()=")
      }
      // Here if encoder fails or `in` is closed
      in.closeIn()
      closeOut()
    } catch {
      case exn: java.io.IOException =>
        if (logging) log.fine(s"transportToNet terminated by: $exn")
        in.closeIn()
        closeOut()
        lastEncoderException = Some(exn)
    }
  }
}

/**
  *  Defines a daemon process `transportFromNet` to transport typed messages from the
  *  network in byte-stream form and deliver them as values.
  *
  * @tparam I the type of data to be input
  */
trait TypedInputTransport[+I] extends Decoder[I] {
  private val log     = TypedTransport.log
  @inline private def logging = log.logging

  /**
    * Returns a daemon process process that, when running, repeatedly reads byte-stream encoded messages from the net, and
    * sends their decoded forms to `out`.
    */
  def transportFromNet(out: !![I]): PROC = proc(this.toString + ".transportFromNet") {
    try {
      repeat {
        val decoded = decode()
        out ! decoded
      }
      if (logging) log.fine(s"transportFromNet terminated by $out closing")
      out.closeOut()
    } catch {
      case exn: java.io.IOException =>
        if (logging) log.fine(s"transportFromNet terminated by: $exn")
        out.closeOut()
        lastDecoderException = Some(exn)
    }
    closeIn()
  }
}

