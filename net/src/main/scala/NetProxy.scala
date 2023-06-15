package ox.net

import io.threadcso.process.PROC
import io.threadcso.{!!, ??, proc, repeat}
import ox.net.codec.{Decoder, Encoder}

object NetProxy {
  val log = ox.logging.Logging.Log()
}

trait NetProxy[-OUT,+IN] extends NetOutputProxy[OUT] with NetInputProxy[IN]

trait NetOutputProxy[-O] extends Encoder[O] {
  private val log     = NetProxy.log
  @inline private def logging = log.logging

  def CopyToNet(in: ??[O]): PROC = proc(this.toString + ".CopyToNet") {
    try {
      repeat {
        val value = in ? ()
        if (logging) log.finest(s"Copy to net toStream($value)")
        encode(value)
        if (logging) log.finest(s"Copy to net encoded()=")
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

trait NetInputProxy[+I] extends Decoder[I] {
  private val log     = NetProxy.log
  @inline private def logging = log.logging

  def CopyFromNet(out: !![I]): PROC = proc(this.toString + ".CopyFromNet") {
    try {
      repeat {
        val decoded = decode()
        out ! decoded
      }
      if (logging) log.fine("CopyFromNet terminated by $out closing")
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

