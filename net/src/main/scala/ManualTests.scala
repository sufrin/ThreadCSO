package ox.net

import app.OPT._
import io.threadcso._
import ox.logging.{Log, Logging => LOGGING}
import ox.net.SSLChannel.{TLSCredential, TLSWithoutCredential, client}
import ox.net.channelfactory.{CRLFChannelFactory, StreamerChannelFactory, UTF8ChannelFactory}
import ox.net.SocketOptions._
import ox.net.UDPChannel.{Datagram, Malformed, UDP}

import java.io.{File, InputStream, OutputStream}
import java.net.{InetAddress, InetSocketAddress, Socket}
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel



/**
 *  Base class for all the manual tests, dealing with
 *  coommand-line flags and parameters.
 */
abstract class ManualTest(doc: String) extends App {
  val logging = true
  var clientauth = false
  val log = ox.logging.Logging.Log("test")
  var factory: TypedChannelFactory[String, String] = UTF8ChannelFactory
  var host: String = "localhost"
  var port: Int    = 10000
  var peerAddr: Option[InetSocketAddress] = None
  var debugPort    = 0
  var SND, RCV     = 0
  var datagram     = false
  var reflect      = true
  var chunked      = true
  var idle         = 5.0
  def idleNS       = seconds(idle)
  var inBufSize, outBufSize = 16 // K
  var bbSize       = 32*1024     // Units, used only for reflect
  var multicastIP  = "224.14.51.6"
  var interfaceName  = "lo0"

  val Command: String = doc
  val Options: List[Opt] = List(
    OPT("-reflect", { reflect = !reflect }, "Don't reflect datagrams to their source (txgrams / rxgrams)"),
    OPT("-datagram", datagram, "Send/receive/reflect datagrams as appropriate"),
    OPT("-crlf", { factory = CRLFChannelFactory }, "Use crlf string protocol"),
    OPT("-utf",  { factory = UTF8ChannelFactory }, "Use utf8 string protocol"),
    OPT("peer://.+:[0-9]+", { case s"peer://$h:$p" => peerAddr = Some(new InetSocketAddress(h, p.toInt) ) }, "Set peer host and port (for p2p)"),
    OPT("//.+:[0-9]+",   { case s"//$h:$p" => host = h; port=p.toInt; () }, "Set host and port"),
    OPT("//.+",          { case s"//$h" => host = h; () }, "Set host"),
    OPT("-auth",         { clientauth = !clientauth}, "authenticate client for an SSL channel (experiment)"),
    OPT("-d",            { debugPort = -1 }, "Disable debugger [enabled at random port otherwise]"),
    OPT("-ch",           { chunked = !chunked }, s"Invert chunked ($chunked)"),
    OPT("-d[0-9]+",      { case s"-d$p" => debugPort = p.toInt}, "Enable debugger on port."),
    OPT("-L[^=]+=[^=]+", { case s"-L${log}=${level}" => LOGGING.setLevel(log, level); () }, "Set log <name>=<level>"),
    OPT("-L=[^=]+",      { case s"-L=${level}" => LOGGING.setLevel("ALL", level); () }, "Set log ALL=<level>"),
    OPT("-SND=[0-9]+",   { case s"-SND=${size}" => SND=size.toInt }, "Network send buffer size"),
    OPT("-RCV=[0-9]+",   { case s"-RCV=${size}" => RCV=size.toInt }, "Network receive buffer size"),
    OPT("-idle", idle,        "<float> set idle time in seconds"),
    OPT("-is=", inBufSize,    "<int>k set input buffer size on channelfactory"),
    OPT("-os=", outBufSize,   "<int>k set output buffer size on channelfactory"),
    OPT("-bb=", bbSize,       "<int> set buffer size for reflect"),
    OPT("-multicast=", multicastIP,  "<multicast-ip-address>"),
    OPT("-if=", interfaceName,  "multicast interface"),

  )

  def Main(): Unit = {
    if (debugPort > 0) System.setProperty("io.threadcso.debugger.port", debugPort.toString)
    if (debugPort >= 0) Console.println(debugger)
    Test()
  }

  def Test(): Unit
}

/**
  * A server whose sessions reflect all packets they receive. Buffers can be of arbitrily small
  * (positive) sizes; and this might be helpful in investigating problems with codecs that could be
  * caused by packet fragmentation.
  *
  * This app is intended to test the "total trip" correctness of codecs by being run "opposite" the apps:
  * {{{
  *   kbd,
  * }}}
  */
object reflect extends ManualTest("reflect - a server that reflects all TCP client packets without interpretation.") {

  def Test(): Unit = {
    def session(channel: SocketChannel): Unit = {
      log.info(s"Accepted: ${channel.getRemoteAddress()}")
      channel.setOption(java.net.StandardSocketOptions.TCP_NODELAY, java.lang.Boolean.TRUE)
      val buffer = ByteBuffer.allocateDirect(bbSize)
      var going = true
      var total = 0
      while (going) {
        val count = channel.read(buffer)
        total += count
        if (logging) log.finest(s"read:: $count/$total")
        buffer.flip()
        channel.write(buffer)
        buffer.clear()
        going = count>0
      }
      channel.close()
    }
    val server = TCPChannel.server(port, 1)(session _)
    server.fork
  }
}

object kbd extends ManualTest("kbd1 -- sends keyboard messages. Run opposite reflect.") {
  def Test() = {
    val channel: TypedTCPChannel[String, String] = ChannelOptions.withOptions(inSize=inBufSize*1024, outSize=outBufSize*1024)
    { TCPChannel.connected(new java.net.InetSocketAddress(host, port), factory) }
    if (SND>0) channel.setOption(SO_SNDBUF, SND)
    if (RCV>0) channel.setOption(SO_RCVBUF, RCV)
    val kbd      = OneOne[String]("kbd")
    val fromHost = OneOneBuf[String](50, name = "fromHost") // A synchronized channel causes deadlock under load
    val toHost   = OneOneBuf[String](50, name = "toHost") // A synchronized channel causes deadlock under load

    // Bootstrap the channel processes
    val toNet        = channel.CopyToNet(toHost).fork
    val fromNet      = channel.CopyFromNet(fromHost).fork
    val fromKeyboard = component.keyboard(kbd, "").fork

    var last: String = ""
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
            if (logging) log.fine(s"toHost ! $last * $times")
            toHost ! (last * times)
          case line =>
            last = line
            if (logging) log.fine(s"toHost ! $line * $times")
            toHost ! (line * times)
        }
      }
      toHost.close()
      toNet.interrupt()
      fromNet.interrupt()
    }
      || proc("fromHost") {
      var n = 0
      repeat {
        n += 1
        val decoded = fromHost ? ()
        if (decoded.isEmpty) {
          log.info(s"Stopping because decoded $n empty")
          stop
        }
        if (decoded.size<50)
          println(f"$n%-3d $decoded")
        else
          println(f"$n%-3d #${decoded.size}%-6d ${decoded.take(20)}...${decoded.drop(decoded.length-20)}")
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

object kbdx extends ManualTest("kbdx -- sends multiple keyboard messages encoded as a datastream of sequences. Run opposite reflect.") {
  type StringArray = Seq[String]
  def Test() = {
    import ox.net.codec.StreamerEncoding._
    implicit object StringSeq extends `Seq*`[String]
    object CF extends StreamerChannelFactory[Seq[String]]
    val channel: TypedTCPChannel[StringArray, StringArray] = ChannelOptions.withOptions(inSize=inBufSize*1024, outSize=outBufSize*1024)
    { TCPChannel.connected(new java.net.InetSocketAddress(host, port), CF) }
    if (SND>0) channel.setOption(SO_SNDBUF, SND)
    if (RCV>0) channel.setOption(SO_RCVBUF, RCV)
    val kbd      = OneOne[String]("kbd")
    val fromHost = OneOneBuf[StringArray](50, name = "fromHost") // A synchronized channel causes deadlock under load
    val toHost   = OneOneBuf[StringArray](50, name = "toHost") // A synchronized channel causes deadlock under load

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
        val out = (for { i<-0 until times } yield s"($i)=$last").toSeq
        log.fine(s"toHost ! $last * $times")
        toHost ! out
      }
      toHost.close()
      toNet.interrupt()
      fromNet.interrupt()
    }
      || proc("fromHost") {
      var n = 0
      repeat {
        n += 1
        val decoded = fromHost ? ()
        if (decoded.length==0)
          println(s"$n: ${decoded.length}")
        else
          println(s"$n: ${decoded.toSeq.take(1)(0)}..${decoded.toSeq.drop(decoded.length-1)(0)}")
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

object txgrams extends ManualTest("txgrams -- sends keyboard datagrams to rxgrams, receives reflected responses") {
  type StringPacket = UDP[String]
  def Test() = {
    // sending on port; receiving on a random port
    val channel = ChannelOptions.withOptions(inSize=inBufSize*1024, outSize=outBufSize*1024) {UDPChannel.connect(host, port, factory) }
    if (SND > 0) channel.setOption(SO_SNDBUF, SND)
    if (RCV > 0) channel.setOption(SO_RCVBUF, RCV)

    val kbd = OneOne[String]("kbd")
    val fromHost = N2NBuf[UDP[String]](50, writers=1, readers=1, name = "fromHost")
    val fromBack = N2NBuf[UDP[String]](50, writers=1, readers=1, name = "fromBack")
    val toHost   = OneOneBuf[UDP[String]](50, name = "toHost")

    // Bootstrap the channel processes
    val toNet        = channel.CopyToNet(toHost).fork
    val fromNet      = channel.CopyFromNet(fromHost).fork
    //val backchannel  = UDPChannel.bind("localhost", port+1, factory)
    //val backFromNet  = backchannel.CopyFromNet(fromBack).fork
    val fromKeyboard = component.keyboard(kbd, "").fork
    var last: String = ""
    var times = 1
    //if (logging) info(s" $channel\n $backchannel")

    run(proc("ui") {
      repeat {
        val line = kbd ? ()
        line match {
          case "." =>
            kbd.close()
            fromKeyboard.interrupt()
            stop
          case s"*$pat" if pat matches("[0-9]+") =>
            times = pat.toInt
            toHost ! Datagram(last*times, channel.getRemoteAddress)
          case line =>
            if (logging) log.fine(s"toHost ! $line * $times")
            last = line
            toHost ! Datagram(line*times, channel.getRemoteAddress)
        }
      }
      toHost.close()
      fromHost.close()
      toNet.interrupt()
      fromNet.interrupt()
      // backFromNet.interrupt()
    }
      || proc("fromHost") {
      var n = 0
      repeat {
        n += 1
        fromHost ? {
          case Datagram(value: String, from) =>
               if (value.size<40) {
                 println(f"$n%-3d $value ($from)")
               } else {
                 println(f"$n%-3d #${value.size}%-6d ($from) ${value.subSequence(0, 30)}...")
               }
          case Malformed(from) =>
            println(f"$n%-3d [MALFORMED PACKET] ($from)")
        }
        ()
      }
      if (logging) log.info(s"Host stopped")
      toHost.closeOut()
      kbd.close()
    }
      || proc("fromBack") {
      var n = 0
      repeat {
        n += 1
        val decoded = fromBack ? ()
        println(s"Back: $decoded")
      }
    }
    )
    kbd.close()
    fromHost.close()
    exit()
  }
}

object rxgrams extends ManualTest("rxgrams receives (and reflects) string datagrams (from txgrams)") {
  import SocketOptions._
  type StringPacket = UDP[String]
  def Test() : Unit =
  { val channel = ChannelOptions.withOptions(inSize=inBufSize*1024, outSize=outBufSize*1024) { UDPChannel.bind(host, port, factory) }
    Console.println(s"$factory ${channel.channel.getLocalAddress}")
    if (RCV>0) channel.setOption(SO_RCVBUF, RCV)
    if (SND>0) channel.setOption(SO_SNDBUF, SND)
    channel.setOption(SO_REUSEADDR, true)

    val fromPeer = OneOneBuf[StringPacket](50, name = "fromPeer")
    val toPeer   = OneOneBuf[StringPacket](50, name = "toPeer")

    // Fork the channel processes
    /**  Handle on the network output proxy. */
    val toNet   = channel.CopyToNet(toPeer).fork
    /** Handle on the network input proxy. */
    val fromNet = channel.CopyFromNet(fromPeer).fork

    val session =
      proc (s"Session($channel") {
        repeat {
          val gram = fromPeer ? ()
          if (logging) log.info(s"fromPeer ? $gram")
          if (reflect) toPeer!gram
        }
      }

    /** Handle on the session. */
    val handle = session.fork
  }
}


/**
  * A point-to-point datagram chat program. Largely to test the `connect`
  * functionality of datagram channels.
  */
object p2p extends ManualTest("p2p -- exchanges datagrams with another p2p") {
  type StringPacket = UDP[String]
  def Test() = {

    val channel = ChannelOptions.withOptions(inSize=inBufSize*1024, outSize=outBufSize*1024) { UDPChannel.bind(host, port, factory) }

    if (SND > 0) channel.setOption(SO_SNDBUF, SND)
    if (RCV > 0) channel.setOption(SO_RCVBUF, RCV)

    val kbd      = OneOne[String]("kbd")
    val fromPeer = N2NBuf[UDP[String]](50, writers=1, readers=1, name = "fromPeer")
    val toPeer   = OneOneBuf[UDP[String]](50, name = "toPeer")

    // Bootstrap the channel processes
    var toNet:   io.threadcso.process.Process.Handle  = {
      channel.connect(peerAddr.get)
      channel.CopyToNet(toPeer).fork
    }
    var fromNet: io.threadcso.process.Process.Handle  = channel.CopyFromNet(fromPeer).fork

    val fromKeyboard = component.keyboard(kbd, "").fork
    var last: String = ""
    var times = 1

    val self = proc("self") {
      if (logging) log.fine(s"Connected to $peerAddr on $channel")
      if (logging) log.fine(s"Relaying to and from $peerAddr")
      repeat {
        val line = kbd ? ()
        line match {
          case "!" =>

          case "." =>
            kbd.close()
            fromKeyboard.interrupt()
            stop
          case s"*$pat" if pat matches("[0-9]+") =>
            times = pat.toInt
            toPeer ! Datagram(last*times, null)
          case line =>
            if (logging) log.fine(s"toPeer ! $line * $times")
            last = line
            toPeer ! Datagram(line*times, null)
        }
      }
      toPeer.close()
      fromPeer.close()
    }

    val peer = proc("peer") {
      var n = 0
      repeat {
        n += 1
        if (logging) log.finest(s"peer listening:")
        fromPeer ? {
          case Datagram(value: String, from) =>
            if (value.size < 40) {
              println(f"$n%-3d $value ($from)")
            } else {
              println(f"$n%-3d #${value.size}%-6d ($from) ${value.subSequence(0, 30)}...")
            }
          case Malformed(from) =>
            println(f"$n%-3d [MALFORMED PACKET] ($from)")
        }
        ()
      }
      if (logging) log.info(s"Host stopped")
      toPeer.closeOut()
      kbd.close()
      fromKeyboard.interrupt()
    }

    run(self || peer)
    exit()
  }


}


/**
  * A trivial https client that sends a `GET / ` and echoes the response to the terminal
  */
object httpsclient extends ManualTest("httpclient -- GETs from a (secure) server then outputs the response line-by-line") {
  def Test() = {
    val credential = if (clientauth) TLSCredential("xyzzyxyzzy", new File("/Users/sufrin/.keystore"))  else TLSWithoutCredential
    val channel = SSLChannel.client(credential, host, port, factory)
    if (SND > 0) channel.setOption(SO_SNDBUF, SND)
    if (RCV > 0) channel.setOption(SO_RCVBUF, RCV)

    val fromServer = OneOneBuf[String](50, name = "fromServer")
    val toServer   = OneOneBuf[String](50, name = "toServer")

    // Bootstrap the channel processes
    val toNet        = channel.CopyToNet(toServer).fork
    val fromNet      = channel.CopyFromNet(fromServer).fork

    val request = proc("request") {
      if (logging) log.fine("Starting request")
      toServer ! "GET / HTTP/1.1"
      toServer ! ""
    }

    /**  Echos a single http response, terminated with an empty line */
    val response = proc("response") {
      if (logging) log.fine("Starting to read response")
      var header = true
      var lineNo = 0
      repeat {
        val line = fromServer ? ()
        line match {
          case "" =>
            if (header) {
              header = false
            } else {
              stop
            }

          case line =>
            if (header) {
               if (logging) log.finest(s"header: $line")
            } else {
              lineNo += 1
              if (logging) log.finest(f"$lineNo%-4d: $line")
            }
        }
      }
      if (logging) log.finest(s"Response concluded")
    }

    // concurrent request/response isn't needed but does no harm
    (request || response)()

    exit()
  }
}

/**
  *  A trivial secure http server that echoes clients' requests back to them as chunk-encoded html text.
  *  The server counts the number of client connections, as well as the number requests made as part of
  *  an individual session before it times-out.
  */
object httpsserver extends ManualTest("httpsserver -- an https server that echoes clients' requests (as html)") {

  def Test() = {
    val serverProcess = SSLChannel.server(SSLChannel.TLSCredential("xyzzyxyzzy", new File("/Users/sufrin/.keystore")), port, factory, clientSession, sync=true, clientAuth=clientauth)
    val theServer = fork(serverProcess)
  }

  var _clientCount: Int = 0

  def clientSession(channel: TypedSSLChannel[String, String]): Unit = {
    val fromClient = OneOneBuf[String](50, name = "fromClient")
    val toClient = OneOneBuf[String](50, name = "toClient")
    val toNet = channel.CopyToNet(toClient).fork
    val fromNet = channel.CopyFromNet(fromClient).fork
    val sessionDate = new java.util.Date().toString
    val remote = channel.getRemoteAddress

    val clientCount = {
      _clientCount += 1
      _clientCount
    }


    log.info(s"New session $clientCount $sessionDate $remote")

    @inline def send(line: String): Unit = toClient ! line

    @inline def headSend(line: String): Unit = {
      if (logging) log.finest(s"http: $line")
      toClient ! line
    }

    @inline def bodySend(line: String): Unit = {
      if (chunked) send("%x".format(line.length))
      if (logging) log.finest(s"body: $line")
      send(line)
    }

    val readBefore: Boolean = false
    val listener = proc("listener") {
      val header = new scala.collection.mutable.Queue[String]
      var requestCount = 0

      def processRequestLine(request: String): Unit = {
        val line = request.trim
        if (logging) log.finest(s"TRIMMED: $line")
        if (line == "")
        // end of the request header reached
        {
          requestCount += 1
          headSend("HTTP/1.1 200 OK")
          headSend("Date: " + sessionDate)
          headSend("Content-Type: text/html")
          //headSend("Connection: close")
          if (chunked) headSend("Transfer-Encoding: chunked")
          headSend("expect: 100-continue")
          headSend("")
          bodySend("<!DOCTYPE html>")
          bodySend(s"<head><title>Session $clientCount#$requestCount</title></head>")
          bodySend("<body>")
          bodySend(s"<h2>Session $clientCount#$requestCount (${new java.util.Date().toString})</h2>")
          bodySend("<ul>")
          while (header.nonEmpty) bodySend(s"<li><tt>${header.dequeue()}</tt></li>")
          bodySend("</ul>")
          bodySend("</body>")
          bodySend("</html>")
          bodySend("")
        }
        else
        // add another line to the header
        {
          header.enqueue(line)
        }
      }

      var running = true
      if (readBefore) {
        while (running)
          fromClient.readBefore(idleNS) match {
            case Some(line) =>
              processRequestLine(line)
            case None =>
              if (logging) log.fine(s"Client $clientCount went idle")
              running = false
          }
      } else
        while (running)
          alt {
            ( fromClient    =?=> { line => processRequestLine(line) }
            | after(idleNS) ==>  { if (logging) log.fine(s"Client $clientCount went idle")
                                   running = false
                                 }
              )
          }

      if (logging) log.info(s"Closing client $clientCount session")

      // Stop reading from the client: the server will eventually find out
      fromClient.closeIn()
      // Probably unnecessary, but must be done after the client close
      toClient.closeOut()
    }


    val listenerHandle = fork(listener)
    if (logging) log.info(s"Listener at $listenerHandle")
  }
}

object reflection extends ManualTest("reflection -- a trivial server that reflects strings sent by its clients") {
    def Test(): Unit = {
    val reflectServer: PROC = TCPChannel.server(port, 0, factory) {
      case channel: TypedTCPChannel[String, String] =>
          val fromClient = OneOne[String](name = "fromClient")
          val toClient = OneOne[String](name = "toClient")
          val toNet = channel.CopyToNet(toClient).fork
          val fromNet = channel.CopyFromNet(fromClient).fork
          fork (proc {
            repeat {
              fromClient ? { text => toClient ! text }
            }
          })
    }
    fork(reflectServer)
  }
}

object timecast extends ManualTest("timecast -- multicast date/time as CRLF text periodically. Run opposite multilisten") {
  def Test(): Unit = {
  val addr      = new InetSocketAddress(InetAddress.getByName(multicastIP), port)
  val multicast = UDPChannel.multicastsTo(interfaceName, addr, CRLFChannelFactory)
    log.info(s"multicast=$multicast, addr=$addr")
    val timeStamps = OneOne[UDP[String]](name="timeStamps")
    val answers    = OneOne[UDP[String]](name="answers")
    val toNet      = multicast.CopyToNet(timeStamps).fork
    val fromNet    = multicast.CopyFromNet(answers).fork
    fork {
      proc ("answers") {
        repeat {
          answers ? { case Datagram(value, addr) => println(s"ANS: $value from $addr"); case udp => println(udp) }
        }
      }
    }
    repeat {
      val now = new java.util.Date(milliTime)
      timeStamps ! Datagram(s"${now}", addr)
      sleep(seconds(5.0))
    }
  }
}

object multilisten extends ManualTest("multilisten -- listen to a multicast channel speaking CRLF. Run opposite timecast") {
  def Test(): Unit = {
    val multicast = UDPChannel.multicastsFrom(interfaceName, multicastIP, port, CRLFChannelFactory)
    val net = OneOne[UDP[String]](name="net")
    val fromNet = multicast.CopyFromNet(net).fork
    serve ( net =?=> {
                  case Datagram(packet, address) => println(s"$packet from $address") ; case m => println(s"??$m??")
            }
          | after(seconds(5.0)) ==> println("...")
          )
  }
}

object interfaces extends ManualTest("interfaces -- list multicast interfaces") {
  def Test(): Unit = {
      import java.net.NetworkInterface._
      val e = getNetworkInterfaces
      while (e.hasMoreElements) {
        val i = e.nextElement()
        def t(f: java.net.NetworkInterface => Boolean, s: String): String =  if (f(i)) s else ""
        println(f"${i.getName}%-10s (${i.getMTU}%d) ${t(_.supportsMulticast, "multicast.")}%s${t(_.isVirtual, "virtual.")}%s${t(_.isLoopback, "loopback.")}%s${t(_.isUp, "up.")}%s${t(_.isPointToPoint, "p2p.")}%s ")
      }
      exit()
  }
}

