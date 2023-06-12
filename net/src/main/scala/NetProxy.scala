package ox.net

import io.threadcso.process.PROC
import io.threadcso.{!!, ??, proc, repeat}
import ox.net.codec.{Decoder, Encoder}

object NetProxy extends ox.logging.Log("NetProxy")

trait NetProxy[-OUT,+IN] extends NetOutputProxy[OUT] with NetInputProxy[IN]

trait NetOutputProxy[-O] extends Encoder[O] {
  private val logging = NetProxy.logging

  def CopyToNet(in: ??[O]): PROC = proc(this.toString + ".CopyToNet") {
    try {
      repeat {
        val value = in ? ()
        if (logging) NetProxy.finest(s"Copy to net toStream($value)")
        encode(value)
        if (logging) NetProxy.finest(s"Copy to net encoded()=")
      }
      // Here if encoder fails or `in` is closed
      in.closeIn()
      closeOut()
    } catch {
      case exn: java.nio.channels.ClosedByInterruptException =>
        if (logging) NetProxy.fine(s"CopyToNet terminated by ClosedInterrupt")
        in.closeIn()
        closeOut()
        lastEncoderException = Some(exn)
    }
  }
}

trait NetInputProxy[+I] extends Decoder[I] {
  private val logging = NetProxy.logging

  def CopyFromNet(out: !![I]): PROC = proc(this.toString + ".CopyFromNet") {
    try {
      repeat {
        val decoded = decode()
        out ! decoded
      }
      if (logging) NetProxy.fine("CopyFromNet terminated")
      out.closeOut()
    } catch {
      case exn: java.io.EOFException =>
        if (logging) NetProxy.fine(s"CopyFromNet terminated by external channel close")
        out.closeOut()
        lastDecoderException = Some(exn)
      case exn: java.nio.channels.ClosedByInterruptException =>
        if (logging) NetProxy.fine(s"CopyFromNet terminated by ClosedInterrupt")
        out.closeOut()
        lastDecoderException = Some(exn)
    }
    closeIn()
  }
}

