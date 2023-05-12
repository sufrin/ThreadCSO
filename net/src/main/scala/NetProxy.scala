package ox.net

import io.threadcso.process.PROC
import io.threadcso.{!!, ??, proc, repeat}
import ox.net.codec.{Decoder, Encoder}

trait NetProxy[OUT,IN] extends NetOutputProxy[OUT] with NetInputProxy[IN]

trait NetOutputProxy[O] extends Encoder[O] {
  def CopyToNet(in: ??[O]): PROC = proc(this.toString + ".CopyToNet") {
    var going = canEncode
    repeat(going) {
      val value = in ? ()
      encode(value)
      going = canEncode
    }
    // Here if encoder fails or in is closed
    in.closeIn()
    closeOut()
  }
}

trait NetInputProxy[I] extends Decoder[I] {
  val log = ox.logging.Log("NIP")
  def CopyFromNet(out: !![I]): PROC = proc(this.toString + ".CopyFromNet") {
    var going = canDecode
    try {
      repeat(going) {
        val decoded = decode()
        going = canDecode
        if (going) {
          out ! decoded
        }
      }
      log.fine("CopyFromNet terminated by decode")
      if (!going) out.closeOut()
    } catch {
      case exn: java.nio.channels.ClosedByInterruptException =>
        log.fine(s"CopyFromNet terminated by ClosedInterrupt")
        out.closeOut()
    }
    closeIn()
  }
}

