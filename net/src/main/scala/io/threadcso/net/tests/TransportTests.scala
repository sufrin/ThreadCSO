package io.threadcso.net.tests

import app.OPT._
import io.threadcso._
import io.threadcso.net.factory.{StreamerTransport, StringTransportCRLF, StringTransportUTF8}
import io.threadcso.net.{SSLTransport, SSLConnection, TCPTransport, TCPConnection, TerminalConnection, UDPTransport, UDPConnection, transport}
import io.threadcso.net.transport.{Options, NetConnection, TypedTransportFactory, TypedTCPTransport}
import io.threadcso.net.SSLTransport.{TLSCredential, TLSWithoutCredential}
import io.threadcso.net.transport.SocketOptions._
import io.threadcso.net.UDPTransport.{Datagram, Malformed, UDP}
import ox.logging.{Logging => LOGGING}
import io.threadcso.net.transport.Options.withOptions

import java.io.File
import java.net.{InetAddress, InetSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel


/**
 *  Base class for most of the manual transport and connection tests, dealing with
 *  coommand-line flags and parameters.
 */
abstract class TransportTest(doc: String) extends App {
  val logging    = true
  var clientauth = false
  val log        = new ox.logging.Log()
  var stringTransportFactory: TypedTransportFactory[String, String] = StringTransportCRLF
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
    OPT("-crlf", { stringTransportFactory = StringTransportCRLF }, "Use crlf string protocol"),
    OPT("-utf",  { stringTransportFactory =  StringTransportUTF8 }, "Use utf8 string protocol"),
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
    OPT("-is=", inBufSize,    "<int>k set input buffer size on factory"),
    OPT("-os=", outBufSize,   "<int>k set output buffer size on factory"),
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

object chat extends TransportTest("chat") {
  //import io.threadcso.net.factory.StringTransportCRLF
  import io.threadcso.net.transport.Connection
  //import io.threadcso._
  def Test(): Unit = {
      println(debugger)
      val netCon: Connection[String, String] = TCPConnection.connected(new java.net.InetSocketAddress(host, port), StringTransportCRLF, "")
      val kbdCon: Connection[String,String] = new TerminalConnection()
      netCon.open()
      kbdCon.open()
      println(s"Net: $netCon")
      kbdCon.out!"> "
      val toKbd = π { repeat { netCon.in ? { line => kbdCon.out ! s"\t$line\n"; kbdCon.out!"> " } }; kbdCon.close() }
      val toNet = π { repeat { kbdCon.in ? { line => netCon.out ! s"\t$line" } }; netCon.close() }
      (toKbd || toNet)()
      exit()
  }
}


object kbd extends TransportTest("kbd - sends one-line keyboard messages. Run opposite reflect, or reflectkbd, or reflectcon.") {
  def Test() = {
    val channel: TypedTCPTransport[String, String] = withOptions(inBufSize=inBufSize*1024, outBufSize=outBufSize*1024)
    { TCPTransport.connected(new java.net.InetSocketAddress(host, port), stringTransportFactory) }
    if (SND>0) channel.setOption(SO_SNDBUF, SND)
    if (RCV>0) channel.setOption(SO_RCVBUF, RCV)
    val kbd      = OneOne[String]("kbd")
    val fromHost = OneOneBuf[String](50, name = "fromHost") // A synchronized channel causes deadlock under load
    val toHost   = OneOneBuf[String](50, name = "toHost") // A synchronized channel causes deadlock under load

    // Bootstrap the channel processes
    val toNet        = channel.transportToNet(toHost).fork
    val fromNet      = channel.transportFromNet(fromHost).fork
    val fromKeyboard = component.keyboard(kbd, "").fork

    var last: String = "?"
    var times = 1

    println("Type lines. (*number or number* replicate the message <number> times; . terminates the program; an empty line terminates the receiver.")

    run(proc("ui") {
      repeat {
        kbd ? () match {
          case "." =>
            kbd.close()
            fromKeyboard.interrupt()
            stop
          case s"$n*" if n matches ("[0-9]+") =>
            times = n.toInt
            if (logging) log.fine(s"toHost ! $last * $times")
            toHost ! (last * times)
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
          log.info(s"Stopping because line $n from host is empty")
          stop
        }
        if (decoded.size<50)
          println(f"$n%-3d $decoded")
        else
          println(f"$n%-3d #${decoded.size}%-6d ${decoded.take(20)}...${decoded.drop(decoded.length-20)}")
      }
      log.info(s"Closing and interrupting")
      kbd.close()
      fromKeyboard.interrupt()
      toNet.interrupt()
      fromNet.interrupt()
      log.info(s"All closed and interrupted")
    }
    )
    kbd.close()
    fromHost.close()
    exit()
  }
}

object kbdcon extends TransportTest("kbdcon - sends single-line keyboard messages. Run opposite reflect, or reflection, or reflectkbd, or reflectcon. Connection API") {
  def Test() = {
    val connection = withOptions(inChanSize=50, outChanSize=50, inBufSize=inBufSize*1024, outBufSize=outBufSize*1024) {
        TCPConnection.connected(new java.net.InetSocketAddress(host, port), stringTransportFactory, "kbdcon")
    }
    if (SND>0) connection.asTCP.setOption(SO_SNDBUF, SND)
    if (RCV>0) connection.asTCP.setOption(SO_RCVBUF, RCV)
    val term = new TerminalConnection("EOF\n")
    val kbd  = term.in
    val fromHost = connection.in
    val toHost   = connection.out
    connection.open()
    term.open()

    var last: String = "?"
    var times = 1

    println("Type lines. (*number or number* replicate the message <number> times; . terminates the program; an empty line terminates the receiver.")

    run(proc("ui") {
      repeat {
        kbd.readBefore(seconds(10.0)) match {
          case None => stop
          case Some(line) =>
            line match {
              case "EOF\n" =>
                term.close()
                stop
              case s"$n*" if n matches ("[0-9]+") =>
                times = n.toInt
                if (logging) log.fine(s"toHost ! $last * $times")
                toHost ! (last * times)
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
      }
      fromHost.closeIn()
      toHost.closeOut()
    }
      || proc("fromHost") {
      var n = 0
      repeat {
        n += 1
        val decoded = fromHost ? ()
        if (decoded.isEmpty) {
          log.info(s"Stopping because line $n from host is empty")
          stop
        }
        if (decoded.size<50)
          println(f"$n%-3d $decoded")
        else
          println(f"$n%-3d #${decoded.size}%-6d ${decoded.take(20)}...${decoded.drop(decoded.length-20)}")
      }
      term.close()
    }
    )
    log.info("Finished")
    term.close()
    exit()
  }
}


object kbds extends TransportTest("kbds -- sends sequences of strings encoded as `Encoding.Seq*`. Run opposite reflect") {
  type Ty = Seq[String]
  def Test() = {
    import io.threadcso.net.streamer.Encoding._
    implicit object `Seq[String]*` extends `Seq*`[String]
    object SequenceTransportFactory$ extends StreamerTransport[Seq[String], Seq[String]]
    val connection: NetConnection[Ty, Ty] =
      withOptions(inBufSize=inBufSize*1024, outBufSize=outBufSize*1024, inChanSize=20, outChanSize=20) {
        TCPConnection.connected(new java.net.InetSocketAddress(host, port), SequenceTransportFactory$, "TCPConnection")
      }

    val fromHost        = connection.in
    val toHost          = connection.out
    val term            = new TerminalConnection("EOF\n")
    val kbd             = term.in
    term.open()
    connection.open()

    var last: String = "?"
    var times = 1

    println("Type lines. (*number or number* sends the message in a sequence of length <number>; . terminates the program)")

    run(proc("ui") {
      repeat {
        kbd ? () match {
          case "EOF\n" =>
            kbd.closeIn()
            stop
          case s"$n*" if n matches ("[0-9]+") =>
            times = n.toInt
          case s"*$n" if n matches ("[0-9]+") =>
            times = n.toInt
          case line =>
            last = line
        }
        val out = (for { i<-0 until times } yield s"($i)=$last").toSeq
        log.fine(s"toHost ! $last * $times")
        toHost ! out
      }
      toHost.closeOut()
      fromHost.closeIn()
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
      kbd.closeIn()
    }
    )
    kbd.closeIn()
    fromHost.closeIn()
    exit()
  }
}


object txgrams extends TransportTest("txgrams -- sends keyboard datagrams to rxgrams, receives reflected responses") {
  type StringPacket = UDP[String]
  def Test() = {
    // sending on port; receiving on a random port
    val channel = withOptions(inBufSize=inBufSize*1024, outBufSize=outBufSize*1024) {
      UDPTransport.connect(host, port, stringTransportFactory)
    }
    if (SND > 0) channel.setOption(SO_SNDBUF, SND)
    if (RCV > 0) channel.setOption(SO_RCVBUF, RCV)

    val kbd = OneOne[String]("kbd")
    val fromHost = N2NBuf[UDP[String]](50, writers=1, readers=1, name = "fromHost")
    val fromBack = N2NBuf[UDP[String]](50, writers=1, readers=1, name = "fromBack")
    val toHost   = OneOneBuf[UDP[String]](50, name = "toHost")

    // Bootstrap the channel processes
    val toNet        = channel.transportToNet(toHost).fork
    val fromNet      = channel.transportFromNet(fromHost).fork
    //val backchannel  = UDPTransport.bind("localhost", port+1, stringTransportFactory)
    //val backFromNet  = backchannel.transportFromNet(fromBack).fork
    val fromKeyboard = component.keyboard(kbd, "").fork
    var last: String = ""
    var times = 1
    //if (logging) info(s" $channel\n $backchannel")

    println(
      """Type lines. (*number or number* replicates the line <number> times as a message; . terminates the program).
        |Overlong messages are expected to cause the datagram transmission to fail; but not txgrams/rxgrams.
        |""".stripMargin)

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

object rxgrams extends TransportTest("rxgrams -- receives (and reflects) string datagrams (from txgrams)") {
  type StringPacket = UDP[String]
  def Test() : Unit =
  { val channel = withOptions(inBufSize=inBufSize*1024, outBufSize=outBufSize*1024) {
      UDPTransport.bind(host, port, stringTransportFactory)
    }
    Console.println(s"$stringTransportFactory ${channel.channel.getLocalAddress}")
    if (RCV>0) channel.setOption(SO_RCVBUF, RCV)
    if (SND>0) channel.setOption(SO_SNDBUF, SND)
    channel.setOption(SO_REUSEADDR, true)

    val fromPeer = OneOneBuf[StringPacket](50, name = "fromPeer")
    val toPeer   = OneOneBuf[StringPacket](50, name = "toPeer")

    // Fork the channel processes
    /**  Handle on the network output daemon. */
    val toNet   = channel.transportToNet(toPeer).fork
    /** Handle on the network input daemon. */
    val fromNet = channel.transportFromNet(fromPeer).fork

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
  * A point-to-point datagram chat program.
  */
object p2p extends TransportTest("p2p -- exchanges datagrams with another p2p") {
  type StringPacket = UDP[String]
  def Test() = {

    val channel = withOptions(inBufSize=inBufSize*1024, outBufSize=outBufSize*1024) { UDPTransport.bind(host, port, stringTransportFactory) }

    if (SND > 0) channel.setOption(SO_SNDBUF, SND)
    if (RCV > 0) channel.setOption(SO_RCVBUF, RCV)

    val kbd      = OneOne[String]("kbd")
    val fromPeer = N2NBuf[UDP[String]](50, writers=1, readers=1, name = "fromPeer")
    val toPeer   = OneOneBuf[UDP[String]](50, name = "toPeer")

    // Bootstrap the channel processes
    var toNet:   io.threadcso.process.Process.Handle  = {
      channel.connect(peerAddr.get)
      channel.transportToNet(toPeer).fork
    }
    var fromNet: io.threadcso.process.Process.Handle  = channel.transportFromNet(fromPeer).fork

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
          case s"$pat*" if pat matches ("[0-9]+") =>
            times = pat.toInt
            toPeer ! Datagram(last * times, null)
          case s"*$pat" if pat matches ("[0-9]+") =>
            times = pat.toInt
            toPeer ! Datagram(last * times, null)
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

    println(
      """Type lines. (*number or number* replicates the line <number> times as a meesage; . terminates the program).
        |Long messages are abbreviated on reception.
        |Overlong messages are expected to cause the datagram transmission to fail; but not txgrams/rxgrams.
        |""".stripMargin)
    run(self || peer)
    exit()
  }


}

object p2pcon extends TransportTest("p2pcon -- exchanges datagrams with another p2p. UDPConnection API") {
  type StringPacket = UDP[String]
  def Test() = {

    val connection = withOptions(inBufSize=inBufSize*1024, outBufSize=outBufSize*1024) {
        UDPConnection.bind(new InetSocketAddress(host, port), stringTransportFactory, "bound")
    }
    if (SND > 0) connection.asUDP.setOption(SO_SNDBUF, SND)
    if (RCV > 0) connection.asUDP.setOption(SO_RCVBUF, RCV)
    val kbd      = OneOne[String]("kbd")
    val fromPeer = connection.in
    val toPeer   = connection.out

    // connect to the peer, and start the daemon
    val daemon   = {
      connection.asUDP.connect(peerAddr.get)
      connection.fork
    }



    val fromKeyboard = component.keyboard(kbd, "").fork
    var last: String = ""
    var times = 1

    val self = proc("self") {
      if (logging) log.fine(s"Connected to $peerAddr on $connection")
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
      toPeer.closeOut()
      fromPeer.closeIn()
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
object httpsclient extends TransportTest("httpclient -- GETs from a (secure) server then outputs the response line-by-line") {
  def Test() = {
    val credential = if (clientauth) TLSCredential("xyzzyxyzzy", new File("/Users/sufrin/.keystore"))  else TLSWithoutCredential
    val connection = withOptions(inChanSize=10, outChanSize=10)
        { SSLConnection.client(credential, host, port, StringTransportCRLF) }
    if (SND > 0) connection.asSSL.setOption(SO_SNDBUF, SND)
    if (RCV > 0) connection.asSSL.setOption(SO_RCVBUF, RCV)
    val fromServer = connection.in
    val toServer   = connection.out
    val daemon = connection.fork

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
object httpsserver extends TransportTest("httpsserver -- an https server that echoes clients' requests (as html)") {
  var _clientCount: Int = 0

  def Test() = {
    val serverProcess = withOptions(inChanSize=10, outChanSize=10) {
      SSLConnection.server(SSLTransport.TLSCredential("xyzzyxyzzy", new File("/Users/sufrin/.keystore")), port, StringTransportCRLF) (forkClientSession)
    }
    run(serverProcess)
  }


  def forkClientSession(connection: NetConnection[String, String]): Unit = {
    clientSession(connection: NetConnection[String, String]).fork
    ()
  }

  def clientSession(connection: NetConnection[String, String]): PROC = proc ("ClientSession $clientCount") {
      val fromClient  = connection.in
      val toClient    = connection.out
      val daemon      = connection.fork
      val sessionDate = new java.util.Date().toString
      val remote      = connection.asSSL.getRemoteAddress

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

/**
  * A server whose sessions reflect all packets they receive. Buffers can be of arbitrily small
  * (positive) sizes; and this might be helpful in investigating problems with codecs that could be
  * caused by packet fragmentation.
  *
  * This app is intended to test the "total trip" correctness of codecs by being run "opposite" various `kbd...`
  * apps.
  */
object reflect extends TransportTest("reflect - a server that reflects all TCP client packets without interpretation.") {

  def Test(): Unit = {
    def session(channel: SocketChannel): Unit = {
      log.info(s"Accepted: ${channel.getRemoteAddress()}")
      channel.setOption(java.net.StandardSocketOptions.TCP_NODELAY, java.lang.Boolean.TRUE)
      val buffer = ByteBuffer.allocateDirect(bbSize)
      var going  = true
      var total  = 0
      while (going) {
        val count = channel.read(buffer)
        total += count
        if (logging) log.finest(s"read:: $count/$total")
        buffer.flip()
        if (logging) log.finest(s"write:: $buffer")
        channel.write(buffer)
        if (logging) log.finest(s"written:: $buffer")
        buffer.clear()
        going = count>0
      }
      channel.close()
    }
    val server = TCPTransport.server(port, 1)(session _)
    server.fork
  }
}

object reflectkbd extends TransportTest("reflectkbd -- a server that reflects strings sent by its client (kbd) ") {
    def Test(): Unit = {
    val reflectServer: PROC = TCPTransport.server(port, 0, stringTransportFactory) {
      case channel: TypedTCPTransport[String, String] =>
          val fromClient = OneOne[String](name = "fromClient")
          val toClient   = OneOne[String](name = "toClient")
          val toNet      = channel.transportToNet(toClient).fork
          val fromNet    = channel.transportFromNet(fromClient).fork
          fork (proc {
            repeat {
              fromClient ? { text => toClient ! text }
            }
          })
    }
    fork(reflectServer)
  }
}

object reflectcon  extends TransportTest("reflectcon -- a TCPConnection-based server that reflects strings sent by its client (kbd).") {
  def Test(): Unit = {
    val reflectServer: PROC = withOptions(inChanSize=10, outChanSize=10){
      TCPConnection.server(port, 0, stringTransportFactory, "Client") {
      case connection: NetConnection[String, String] =>
        connection.open()
        log.info(s"Session for $connection")
        fork (proc {
          repeat {
            connection.in ? { text =>
              log.fine(s"> $text")
              connection.out ! text
            }
          }
        })
      }
    }
    fork(reflectServer)
  }
}

object timecast extends TransportTest("timecast -- multicast date/time as CRLF text periodically. Run opposite multilisten") {
  def Test(): Unit = {
  val addr      = new InetSocketAddress(InetAddress.getByName(multicastIP), port)
  val multicast = UDPTransport.multicastsTo(interfaceName, addr, StringTransportCRLF)
    log.info(s"multicast=$multicast, addr=$addr")
    val connection = transport.NetConnection[UDP[String],UDP[String]](multicast)
    val timeStamps = connection.out // OneOne[UDP[String]](name="timeStamps")
    val answers    = connection.in // OneOne[UDP[String]](name="answers")
    //val toNet      = multicast.transportToNet(timeStamps).fork
    //val fromNet    = multicast.transportFromNet(answers).fork
    val daemon = connection.fork
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

object multilisten extends TransportTest("multilisten -- listen to a multicast channel speaking CRLF. Run opposite timecast") {
  def Test(): Unit = {
    val multicast = UDPTransport.multicastsFrom(interfaceName, multicastIP, port, StringTransportCRLF)
    val net = OneOne[UDP[String]](name="netchannels")
    val fromNet = multicast.transportFromNet(net).fork
    serve ( net =?=> {
                  case Datagram(packet, address) => println(s"$packet from $address") ; case m => println(s"??$m??")
            }
          | after(seconds(5.0)) ==> println("...")
          )
  }
}

object interfaces extends TransportTest("interfaces -- list multicast interfaces") {
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


/* Sends `StreamEncoding.Seq*` encoded messages. Superseded by `kbds`
object kbdd extends TransportTest("kbdd -- sends multiple keyboard messages encoded as a datastream of sequences. Run opposite reflect.") {
  type Ty = Seq[String]
  def Test() = {
    import io.threadcso.netchannels.streamer.Encoding._
    implicit object `Seq[String]*` extends `Seq*`[String]
    object CF extends StreamerTransport[Seq[String], Seq[String]]
    val channel: TypedTCPTransport[Ty, Ty] = withOptions(inBufSize=inBufSize*1024, outBufSize=outBufSize*1024)
    { TCPTransport.connected(new java.netchannels.InetSocketAddress(host, port), CF) }
    if (SND>0) channel.setOption(SO_SNDBUF, SND)
    if (RCV>0) channel.setOption(SO_RCVBUF, RCV)
    val kbd      = OneOne[String]("kbd")
    val fromHost = OneOneBuf[Ty](50, name = "fromHost") // A synchronized channel causes deadlock under load
    val toHost   = OneOneBuf[Ty](50, name = "toHost") // A synchronized channel causes deadlock under load
    // Bootstrap the channel processes
    val toNet        = channel.transportToNet(toHost).fork
    val fromNet      = channel.transportFromNet(fromHost).fork
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
*/

