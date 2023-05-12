package ox.net

import app.OPT._
import io.threadcso._
import ox.logging.{Log, Logging => LOGGING}
import ox.net.SSLChannel.TLSCredential
import ox.net.channelfactory.{CRLFChannelFactory, UTF8ChannelFactory}
import ox.net.httpclient.{factory, host, log, port}
import ox.net.SocketOptions._
import ox.net.codec.Codec
import ox.net.kbdgrams.{inBufSize, outBufSize}

import java.io.{File, InputStream, OutputStream}
import java.net.{InetAddress, InetSocketAddress, Socket}
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel

/**
 *  Base class for all the manual tests, dealing with
 *  coommand-line flags and parameters.
 */
abstract class ManualTest(doc: String) extends App {
  var factory: TypedChannelFactory[String, String] = UTF8ChannelFactory
  var host: String = "localhost"
  var port: Int    = 10000
  var debugPort    = 0
  var SND, RCV     = 0
  var datagram     = false
  var chunked      = true
  var idle         = 5.0
  def idleNS       = seconds(idle)
  var inBufSize, outBufSize = 16 // K
  var bbSize       = 32*1024     // Units, used only for reflect

  val Command: String = doc
  val Options: List[Opt] = List(
    OPT("-datagram", datagram, "Send/receive/reflect datagrams as appropriate"),
    OPT("-crlf", { factory = CRLFChannelFactory }, "Use crlf string protocol"),
    OPT("-utf",  { factory = UTF8ChannelFactory }, "Use utf8 string protocol"),
    OPT("//.+:[0-9]+",   { case s"//$h:$p" => host = h; port=p.toInt; () }, "Set host and port"),
    OPT("//.+",          { case s"//$h" => host = h; () }, "Set host"),
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

  )

  val log = Log("log")

  def Main(): Unit = {
    if (debugPort > 0) System.setProperty("io.threadcso.debugger.port", debugPort.toString)
    if (debugPort >= 0) Console.println(debugger)
    Test()
  }

  def Test(): Unit

}

object reflect extends ManualTest("reflect - a server that reflects all TCP packets without interpretation.") {
  /**
    *  A server whose sessions reflect all packets they receive. Buffers can be of arbitrily small
    *  (positive) sizes; and this might be helpful in investigating problems with codecs that could be
    *  caused by packet fragmentation.
    */
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
        log.info(s"read:: $count/$total")
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

object kbd extends ManualTest("kbd -- sends keyboard messages, receives responses") {
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
                log.fine(s"toHost ! $last * $times")
                toHost ! (last * times)
              case line =>
                last = line
                log.fine(s"toHost ! $line * $times")
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
              log.finest(s"Stopping because decoded $n empty")
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

object kbdgrams extends ManualTest("kbdgrams -- sends keyboard datagrams, receives responses") {
  type StringPacket = Packet[String]
  def Test() = {
    // sending on port; receiving on a random port
    val channel = ChannelOptions.withOptions(inSize=inBufSize*1024, outSize=outBufSize*1024) {UDPChannel.connect(host, port, factory) }
    if (SND > 0) channel.setOption(SO_SNDBUF, SND)
    if (RCV > 0) channel.setOption(SO_RCVBUF, RCV)

    val kbd = OneOne[String]("kbd")
    val fromHost = N2NBuf[StringPacket](50, writers=1, readers=1, name = "fromHost") // A synchronized channel causes deadlock under load
    val fromBack = N2NBuf[StringPacket](50, writers=1, readers=1, name = "fromBack") // A synchronized channel causes deadlock under load
    val toHost   = OneOneBuf[StringPacket](50, name = "toHost") // A synchronized channel causes deadlock under load

    // Bootstrap the channel processes
    val toNet        = channel.CopyToNet(toHost).fork
    val fromNet      = channel.CopyFromNet(fromHost).fork
    val backchannel  = UDPChannel.bind("localhost", port+1, factory)
    val backFromNet  = backchannel.CopyFromNet(fromBack).fork
    val fromKeyboard = component.keyboard(kbd, "").fork
    var last: String = ""
    var times = 1
    log.info(s" $channel\n $backchannel")

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
            toHost ! Packet(last*times, channel.getRemoteAddress)
          case line =>
            log.fine(s"toHost ! $line * $times")
            last = line
            toHost ! Packet(line*times, channel.getRemoteAddress)
        }
      }
      toHost.close()
      fromHost.close()
      toNet.interrupt()
      fromNet.interrupt()
      backFromNet.interrupt()
    }
      || proc("fromHost") {
      var n = 0
      repeat {
        n += 1
        val decoded = fromHost ? ()
        if (times<4)
           println(f"$n%-3d $decoded")
        else
           println(f"$n%-3d #${decoded.value.size}%-6d ${decoded.address}")
      }
      log.info(s"Host stopped")
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

object receivedatagrams extends ManualTest("receivedatagrams receives string datagrams") {
  import SocketOptions._
  type StringPacket = Packet[String]
  def Test() : Unit =
  { val channel = ChannelOptions.withOptions(inSize=inBufSize*1024, outSize=outBufSize*1024) { UDPChannel.bind(host, port, factory) }
    Console.println(channel.channel.getLocalAddress)
    if (RCV>0) channel.setOption(SO_RCVBUF, RCV)
    if (SND>0) channel.setOption(SO_SNDBUF, SND)
    channel.setOption(SO_REUSEADDR, true)

    val fromHost = OneOneBuf[StringPacket](50, name = "fromHost") // A synchronized channel causes deadlock under load
    val toHost   = OneOneBuf[StringPacket](50, name = "toHost") // A synchronized channel causes deadlock under load

    // Fork the channel processes
    /**  Handle on the network output proxy. */
    val toNet   = channel.CopyToNet(toHost).fork
    /** Handle on the network input proxy. */
    val fromNet = channel.CopyFromNet(fromHost).fork

    val session =
      proc (s"Session($channel") {
        repeat {
          val gram = fromHost ? ()
          log.info(s"fromHost ? $gram")
          if (datagram) toHost!gram
        }
      }

    /** Handle on the session. */
    val handle = session.fork
  }
}

object httpclient extends ManualTest("httpclient -- GETs from a server then outputs the response line-by-line") {
  def Test() = {
    val channel = SSLChannel.client(TLSCredential(null, null), host, port, factory)
    if (SND > 0) channel.setOption(SO_SNDBUF, SND)
    if (RCV > 0) channel.setOption(SO_RCVBUF, RCV)

    val fromHost = OneOneBuf[String](50, name = "fromHost") // A synchronized channel causes deadlock under load
    val toHost   = OneOneBuf[String](50, name = "toHost") // A synchronized channel causes deadlock under load

    // Bootstrap the channel processes
    val toNet        = channel.CopyToNet(toHost).fork
    val fromNet      = channel.CopyFromNet(fromHost).fork

    val request = proc("request") {
      log.fine("Starting request")
      toHost ! "GET / HTTP/1.1"
      toHost ! ""
    }

    /**  Echos a single http response, terminated with an empty line */
    val response = proc("response") {
      log.fine("Starting to read response")
      var header = true
      var lineNo = 0
      repeat {
        val line = fromHost ? ()
        line match {
          case "" =>
            if (header) {
              header = false
            } else {
              stop
            }

          case line =>
            if (header) {
               log.finest(s"header: $line")
            } else {
              lineNo += 1
              log.finest(f"$lineNo%-4d: $line")
            }
        }
      }
      log.finest(s"Response concluded")
    }
    (request || response)()
    exit()
  }
}

object httpserver extends ManualTest("httpserver -- core of an https server") {

  def Test() = {
    val serverProcess = SSLChannel.server(SSLChannel.TLSCredential("xyzzyxyzzy", new File("/Users/sufrin/.keystore")), port, factory, clientSession)
    val theServer = fork(serverProcess)
  }

  var _clientCount: Int = 0

  def clientSession(channel: TypedSSLChannel[String,String]): Unit = {
    val fromClient   = OneOneBuf[String](50, name = "fromClient")
    val toClient     = OneOneBuf[String](50, name = "toClient")
    val toNet        = channel.CopyToNet(toClient).fork
    val fromNet      = channel.CopyFromNet(fromClient).fork
    val sessionDate  = new java.util.Date().toString
    val remote       = channel.getRemoteAddress

    val clientCount = {
      _clientCount += 1
      _clientCount
    }


    log.info(s"New session $clientCount $sessionDate $remote")

    @inline def send(line: String): Unit = toClient!line

    @inline def headSend(line: String): Unit = {
      log.finest(s"http: $line")
      toClient!line
    }

    @inline def bodySend(line: String): Unit = {
      if (chunked) send("%x".format(line.length))
      log.finest(s"body: $line")
      send(line)
    }

    val readBefore: Boolean = false
    val listener = proc("listener") {
      val header = new scala.collection.mutable.Queue[String]
      var requestCount = 0

      def processRequestLine(request: String): Unit = {
        val line = request.trim
        log.finest(s"TRIMMED: $line")
        if (line == "")
        // end of the request header reached
        { requestCount += 1
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
              log.fine(s"Client $clientCount went idle")
              running = false
          }
      } else
        while (running)
          alt {(fromClient =?=>
                   { line =>
                       processRequestLine(line)
                   }
               | after(idleNS) ==> {
                   log.fine(s"Client $clientCount went idle")
                   running = false
                 }
              )}

      log.info(s"Closing client $clientCount session")

      // Stop reading from the client: the server will eventually find out
      fromClient.closeIn()
      // Probably unnecessary, but must be done after the client close
      toClient.closeOut()
    }


    val listenerHandle = fork(listener)
    log.info(s"Listener at $listenerHandle")
  }
}

/*
  def testServer(): Unit = {
    ssl.info(s"Serving $host:$port")
    val socketSpec = if (secure) TLSCredential("xyzzyxyzzy", new File("/Users/sufrin/.keystore")) else NetSocket
    val socket = serverSocket(socketSpec, port)
    val serverProcess = server(socket, newSession).withName("ServerProcess")
    ssl.info(s"server at $port")
    serverProcess()
  }
  var count: Int = 0
  def newSession(client: ClientConnection[String, String]): Unit = {
    val serverDate = new java.util.Date().toString
    val fromNet = client.fromClient
    val toNet = client.toClient
    val clientnum = {
      count += 1; count
    }
    log.info("New client %d".format(clientnum))
    @inline def chunk(line: String): Unit = {
      send("%x".format(line.length))
      log.finest(s"http: '$line''")
      send(line)
    }
    @inline def send(line: String): Unit = {
      toNet ! line
    }
    /** Whether to use the time-bounded `alt` or the `readBefore`
      * when reading `fromNet`.
      *
      * TODO: understand why when BOUND was 10, the `readBefore` route
      * deadlocked; yet the `alt` route didn't.
      * It's dangerous to have an unbounded `fromNet`, but what
      * should the `BOUND` be? (100 worked on my tests, ....)
      */

    val handler =
      fork(π {
        log.info("serving")
        listener()
        log.info("serve %d: terminated".format(clientnum))
      })
    log.info("Handler spawned")
  }

 */
