
package ox.net

import io.threadcso.{OneOne, OneOneBuf, component, exit, proc, repeat, run, stop}
import ox.net.SocketOptions.{SO_RCVBUF, SO_SNDBUF}
import ox.net.channelfactory.DataStreamChannelFactory


/**
  * A very cursory test of sending and receiving messages encoded using `DataStreamEncoding` codecs.
  *
  * Run this concurrently with `ox.net.reflect`  to test the total-trip correctness
  */

object dskbd extends ManualTest("dskbd -- sends sequences of records ") {

  import ox.net.codec.DataStreamEncoding.Primitive._
  import ox.net.codec.DataStreamEncoding._

  case class Record(name: String, value: Seq[Span])
  case class Span(low: Int, high: Int)

  // implicit evidence of data stream encodings
  implicit object `Span*`       extends `2cons`[Span, Int, Int](Span.apply, Span.unapply)
  implicit object `SpanSeq*`    extends Sequence[Span]
  implicit object `Record*`     extends `2cons`[Record, String, Seq[Span]](Record.apply, Record.unapply)
  implicit object `RecordSeq*`  extends Sequence[Record]


  type Ty = Seq[Record]

  def Test() = {
    val channel: TypedTCPChannel[Ty, Ty] = ChannelOptions.withOptions(inSize=inBufSize*1024, outSize=outBufSize*1024)
    {
       TCPChannel.connected(new java.net.InetSocketAddress(host, port), new DataStreamChannelFactory[Ty]() )
    }

    if (SND>0) channel.setOption(SO_SNDBUF, SND)
    if (RCV>0) channel.setOption(SO_RCVBUF, RCV)
    val kbd      = OneOne[String]("kbd")
    val fromHost = OneOneBuf[Ty](50, name = "fromHost") // A synchronized channel causes deadlock under load
    val toHost   = OneOneBuf[Ty](50, name = "toHost") // A synchronized channel causes deadlock under load

    // Bootstrap the channel processes
    val toNet        = channel.CopyToNet(toHost).fork
    val fromNet      = channel.CopyFromNet(fromHost).fork
    val fromKeyboard = component.keyboard(kbd, "").fork

    var last: String = "?"
    var times = 1


    run(proc("self") {
      repeat {
        kbd ? () match {
          case "." =>
            kbd.close()
            fromKeyboard.interrupt()
            stop
          case s"$n*" if n matches ("[0-9]+") =>
            times = n.toInt
          case s"*$n" if n matches ("[0-9]+") =>
            times = n.toInt
          case line =>
            last = line
        }
        log.fine(s"toHost ! $last * $times")
        toHost ! ( for { i<-0 until times } yield Record(last, List(Span(i, i+10))))
      }
      toHost.close()
      toNet.interrupt()
      fromNet.interrupt()
    }
      || proc("reflected") {
      var n = 0
      repeat {
        n += 1
        val decoded = (fromHost ? () )
        if (decoded.length == 0)
          println(s"$n: ${decoded.length}")
        else
          println(s"$n: ${decoded.toSeq.take(1)(0)}..${decoded.toSeq.drop(decoded.length - 1)(0)}")
      }
      toHost.closeOut()
      kbd.close()
    }
    )
    kbd.close()
    fromHost.close()
    exit()
  }
}

