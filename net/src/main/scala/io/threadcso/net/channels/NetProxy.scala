package io.threadcso.net.channels

import io.threadcso.net.codec.{Decoder, Encoder}
import io.threadcso.process.PROC
import io.threadcso.{!!, ??, proc, repeat}

object NetProxy {
  val log = ox.logging.Logging.Log()
}

/**
  *  Implicitly associated with a pair of network byte-streams. Provides daemon processes to encode and copy typed messages to the
  *  network and to decode messages from the network.
  */
trait NetProxy[-OUT,+IN] extends NetOutputProxy[OUT] with NetInputProxy[IN] {
  override def toString: String = "NetProxy"
}

/**
  *  Defines a daemon process `CopyToNet` to copy typed messages to the
  *  network in byte-stream form.
  * @tparam O the type of data to be output by the encoder
  */
trait NetOutputProxy[-O] extends Encoder[O] {
  private val log     = NetProxy.log
  @inline private def logging = log.logging

  /**
    * Returns a process that repeatedly reads messages from `in` as then sends
    * their byte-stream-form to the network.
    */
  def CopyToNet(in: ??[O]): PROC = proc(this.toString + ".CopyToNet") {
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
        if (logging) log.fine(s"CopyToNet terminated by: $exn")
        in.closeIn()
        closeOut()
        lastEncoderException = Some(exn)
    }
  }
}

/**
  *  Defines a daemon process `CopyFromNet` to copy typed messages from the
  *  network in byte-stream form.
  *
  * @tparam I the type of data to be input
  */
trait NetInputProxy[+I] extends Decoder[I] {
  private val log     = NetProxy.log
  @inline private def logging = log.logging

  /**
    * Returns a process that repeatedly reads byte-stream encoded messages from the net, and
    * sends their decoded forms to `out`.
    */
  def CopyFromNet(out: !![I]): PROC = proc(this.toString + ".CopyFromNet") {
    try {
      repeat {
        val decoded = decode()
        out ! decoded
      }
      if (logging) log.fine(s"CopyFromNet terminated by $out closing")
      out.closeOut()
    } catch {
      case exn: java.io.IOException =>
        if (logging) log.fine(s"CopyFromNet terminated by: $exn")
        out.closeOut()
        lastDecoderException = Some(exn)
    }
    closeIn()
  }
}

