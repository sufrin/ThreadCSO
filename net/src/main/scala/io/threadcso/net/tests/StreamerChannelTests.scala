
package io.threadcso.net.tests

import io.threadcso.net.TCPChannel
import io.threadcso.net.factory.StreamerChannel
import io.threadcso.net.channels.{ChannelOptions, TypedTCPChannel}
import io.threadcso.net.channels.SocketOptions.{SO_RCVBUF, SO_SNDBUF}
import io.threadcso.{OneOne, OneOneBuf, component, exit, proc, repeat, run, stop}


/**
  * A cursory test for the sending and receiving of messages encoded using `Encoding`
  * codecs.
  *
  * `times` is the number of times each message is replicated. It can be set from the keyboard
  * by typing `*`digits or digits`*`.
  *
  * Any other line typed at the keyboard is sent as a sequence of length `times` recorss. If
  * `times` is even the records are of type `Record1`, else the records are of type `Record2`.
  *
  * Run this concurrently with `io.threadcso.netchannels.channels.netchannels.reflect`  to test total-trip correctness.
  */

object dskbd extends ManualTest("dskbd -- sends sequences of records ") {

  import io.threadcso.net.streamer.Encoding._

  trait Record
  case class Record1(name: String, value: Seq[Span]) extends Record
  case class Record2(name: String, size: Int) extends Record
  case class Span(low: Int, high: Int)
  type RecordSeq = Seq[Record]


  // implicit data stream encodings: you try naming them!
  implicit object `Span*`       extends `2-Case*`[Span, Int, Int](Span.apply, Span.unapply)
  implicit object `SpanSeq*`    extends `Seq*`[Span]
  implicit object `Record1*`    extends `2-Case*`[Record1, String, Seq[Span]](Record1.apply, Record1.unapply)
  implicit object `Record2*`    extends `2-Case*`[Record2, String, Int](Record2.apply, Record2.unapply)
  implicit object `Record*`     extends `2-Union*`[Record, Record1, Record2](_.asInstanceOf[Record1], _.asInstanceOf[Record2])
  implicit object `RecordSeq*`  extends `Seq*`[Record]

  def Test() = {
    val channel: TypedTCPChannel[RecordSeq, RecordSeq] = ChannelOptions.withOptions(inBufSize=inBufSize*1024, outBufSize=outBufSize*1024)
    {
       TCPChannel.connected(new java.net.InetSocketAddress(host, port), new StreamerChannel[RecordSeq, RecordSeq]())
    }

    if (SND>0) channel.setOption(SO_SNDBUF, SND)
    if (RCV>0) channel.setOption(SO_RCVBUF, RCV)
    val kbd      = OneOne[String]("kbd")
    val fromHost = OneOneBuf[RecordSeq](50, name = "fromHost")
    val toHost   = OneOneBuf[RecordSeq](50, name = "toHost")

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
        toHost !( for { i<-0 until times } yield
               { val r: Record = if (times % 2==0) Record1(last, List(Span(i, i+10))) else Record2(last, times)
                 r
               })
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

