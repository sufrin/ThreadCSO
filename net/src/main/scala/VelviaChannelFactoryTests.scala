
package ox.net

import io.threadcso.{OneOne, OneOneBuf, component, exit, proc, repeat, run, stop}
import ox.net.SocketOptions.{SO_RCVBUF, SO_SNDBUF}
import ox.net.channelfactory.VelviaChannelFactory


/**
  * (*) It can be just a little tedious to deploy the machinery
  * of implicit inference of codecs. It might be more straightforward if all built-in
  * implicits were available at a single import.
  *
  * (*) To be precise: it seems a shame that the obvious implicit codec derivations for
  * `Ty` cannot be derived just from the definition of that type in the
  * three examples below.
  */

object kbdy extends ManualTest("kbdy -- sends multiple keyboard messages (using msgpack), receives responses") {

  import org.velvia.msgpack._
  import RawStringCodecs._
  import SimpleCodecs._
  import TupleCodecs._

    type Ty = (Int, Int)
    implicit object TyCodec extends TupleCodec2[Int,Int]

    def Test() = {
      val channel: TypedTCPChannel[Ty, Ty] = ChannelOptions.withOptions(inSize=inBufSize*1024, outSize=outBufSize*1024)
      { TCPChannel.connected(new java.net.InetSocketAddress(host, port), new VelviaChannelFactory[Ty]) }

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


      run(proc("ui") {
        repeat {
          kbd ? () match {
            case "." =>
              kbd.close()
              fromKeyboard.interrupt()
              stop
            case s"*$n" if n matches ("[0-9]+") =>
              times = n.toInt
            case line =>
              last = line
          }
          log.fine(s"toHost ! $last * $times")
          toHost ! ((times, last.length))
        }
        toHost.close()
        toNet.interrupt()
        fromNet.interrupt()
      }
        || proc("fromHost") {
        var n = 0
        repeat {
          n += 1
          val decoded = (fromHost ? () )
            println(s"$n: ${decoded}")
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

object kbdz extends ManualTest("kbdz -- sends multiple keyboard messages (as pairs) (using msgpack), receives responses") {

  import org.velvia.msgpack._
  import RawStringCodecs._
  import SimpleCodecs._
  import TupleCodecs._

  type Ty = (Int, String)

  implicit object TyCodec extends TupleCodec2[Int, String]

  def Test() = {
    val channel: TypedTCPChannel[Ty, Ty] = ChannelOptions.withOptions(inSize = inBufSize * 1024, outSize = outBufSize * 1024) {
      TCPChannel.connected(new java.net.InetSocketAddress(host, port), new VelviaChannelFactory[Ty])
    }

    if (SND > 0) channel.setOption(SO_SNDBUF, SND)
    if (RCV > 0) channel.setOption(SO_RCVBUF, RCV)
    val kbd = OneOne[String]("kbd")
    val fromHost = OneOneBuf[Ty](50, name = "fromHost") // A synchronized channel causes deadlock under load
    val toHost = OneOneBuf[Ty](50, name = "toHost") // A synchronized channel causes deadlock under load

    // Bootstrap the channel processes
    val toNet = channel.CopyToNet(toHost).fork
    val fromNet = channel.CopyFromNet(fromHost).fork
    val fromKeyboard = component.keyboard(kbd, "").fork

    var last: String = "?"
    var times = 1


    run(proc("ui") {
      repeat {
        kbd ? () match {
          case "." =>
            kbd.close()
            fromKeyboard.interrupt()
            stop
          case s"*$n" if n matches ("[0-9]+") =>
            times = n.toInt
          case line =>
            last = line
        }
        log.fine(s"toHost ! $last * $times")
        toHost ! ((times, last*times))
      }
      toHost.close()
      toNet.interrupt()
      fromNet.interrupt()
    }
      || proc("fromHost") {
      var n = 0
      repeat {
        n += 1
        val (times, decoded) = (fromHost ? ())
        if (decoded.length == 0)
          println(s"$n: $times ${decoded.length}")
        else
          println(s"$n: $times ${decoded.toSeq.take(1)(0)}..${decoded.toSeq.drop(decoded.length - 1)(0)}")
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

object kbdq extends ManualTest("kbdq -- sends multiple keyboard messages wrapped in a case class (using msgpack), receives responses") {

  import org.velvia.msgpack._
  import CaseClassCodecs._
  import RawStringCodecs._
  import SimpleCodecs._
  import TupleCodecs._

  case class Ty(times: Int, string: String)
  implicit object TyCodec extends CaseClassCodec2[Ty, Int, String](Ty, { t => Some(t.times, t.string)})

  def Test() = {
    val channel: TypedTCPChannel[Ty, Ty] = ChannelOptions.withOptions(inSize = inBufSize * 1024, outSize = outBufSize * 1024) {
      TCPChannel.connected(new java.net.InetSocketAddress(host, port), new VelviaChannelFactory[Ty])
    }

    if (SND > 0) channel.setOption(SO_SNDBUF, SND)
    if (RCV > 0) channel.setOption(SO_RCVBUF, RCV)
    val kbd = OneOne[String]("kbd")
    val fromHost = OneOneBuf[Ty](50, name = "fromHost") // A synchronized channel causes deadlock under load
    val toHost = OneOneBuf[Ty](50, name = "toHost") // A synchronized channel causes deadlock under load

    // Bootstrap the channel processes
    val toNet = channel.CopyToNet(toHost).fork
    val fromNet = channel.CopyFromNet(fromHost).fork
    val fromKeyboard = component.keyboard(kbd, "").fork

    var last: String = "?"
    var times = 1


    run(proc("ui") {
      repeat {
        kbd ? () match {
          case "." =>
            kbd.close()
            fromKeyboard.interrupt()
            stop
          case s"*$n" if n matches ("[0-9]+") =>
            times = n.toInt
          case line =>
            last = line
        }
        log.fine(s"toHost ! $last * $times")
        toHost ! Ty(times, last*times)
      }
      toHost.close()
      toNet.interrupt()
      fromNet.interrupt()
    }
      || proc("fromHost") {
      var n = 0
      repeat {
        n += 1
        val Ty(times, decoded) = (fromHost ? ())
        if (decoded.length == 0)
          println(s"$n: $times ${decoded.length}")
        else
          println(s"$n: $times ${decoded.toSeq.take(1)(0)}..${decoded.toSeq.drop(decoded.length - 1)(0)}")
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
