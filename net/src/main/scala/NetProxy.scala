package ox.net

import io.threadcso.process.PROC
import io.threadcso.{!!, ??, proc, repeat}
import ox.net.codec.{Decoder, Encoder}

object NetProxy extends ox.logging.Log("NetProxy")

trait NetProxy[-OUT,+IN] extends NetOutputProxy[OUT] with NetInputProxy[IN]

trait NetOutputProxy[-O] extends Encoder[O] {

  def CopyToNet(in: ??[O]): PROC = proc(this.toString + ".CopyToNet") {
    try {
      repeat {
        val value = in ? ()
        encode(value)
      }
      // Here if encoder fails or `in` is closed
      in.closeIn()
      closeOut()
    } catch {
      case exn: java.nio.channels.ClosedByInterruptException =>
        NetProxy.fine(s"CopyToNet terminated by ClosedInterrupt")
        in.closeIn()
        closeOut()
        lastEncoderException = Some(exn)
    }
  }
}

trait NetInputProxy[+I] extends Decoder[I] {
  def CopyFromNet(out: !![I]): PROC = proc(this.toString + ".CopyFromNet") {
    try {
      repeat {
        val decoded = decode()
        out ! decoded
      }
      NetProxy.fine("CopyFromNet terminated")
      out.closeOut()
    } catch {
      case exn: java.nio.channels.ClosedByInterruptException =>
        NetProxy.fine(s"CopyFromNet terminated by ClosedInterrupt")
        out.closeOut()
        lastDecoderException = Some(exn)
    }
    closeIn()
  }
}

