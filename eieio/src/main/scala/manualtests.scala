import app.OPT._
import io.threadcso._
import io.threadcso.channel.Closed
import io.threadcso.process.CSOThreads.{UNPOOLED, VIRTUAL}
import ox.eieio._
import ox.eieio.codecs._
import ox.eieio.options._
import ox.eieio.types._
import ox.logging.{Log, Logging => LOGGING}

import java.io.{InputStream, InputStreamReader}
import java.net.{InetAddress, InetSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import scala.collection.mutable

// import Thing._ // Awaiting 2.12 Scala pickler

/** 
        Some systematic simple tests for Eieio

       http
*/

/**
        All examples extend this class. It deals with command line argument
        parsing uniformly; though not all examples need all the
        options/switches it can parse.
*/
abstract class ManualTest(doc: String) extends App
{
    var port       = 10000
    var host       = "localhost"
    var multicast  = "224.1.25.25"
    var copies     = 100
    var sync       = false
    var delay      = 500L
    var bufferSize = 1024
    var relay      = false
    var sendStrings= false
    var nodelay    = false
    var keepAlive  = false
    var listenSize = 0
    var httpidle   = 5.0
    var SND        = 2048
    var RCV        = 2048
    var debugging  = 0
    var constant   = false
    val nonSwitch  = new scala.collection.mutable.Queue[String]
    var level      = "info"
    var json       = false
    var TTL        = 0
    var vpkt, ipkt, crlf, nul, utf = false
    lazy val stringCodec: SimpleCodec[String] =
                                if (vpkt)  VarIntPacketString else
                                if (ipkt)  IntPacketString    else
                                if (crlf)  CRLFString         else
                                if (nul)   NULString          else
                                           LFString
    /* Awaiting 2.12 Scala pickler
    lazy val pickleCodec: SimpleCodec[Thing] =
                                if (json) JSONThingPickle else
                                          StaticThingPickle
    */
    val Command: String = doc
    val Options: List[Opt] = List (
      OPT("-help", { Usage() }, "prints usage text")
    , OPT("-host", host,      s"hostname [$host]")
    , OPT("-p", port,         s"<port> portnumber [$port]")
    , OPT("-m", multicast,    s"<multicastaddress> set multicast address [$multicast]")
    , OPT("-ttl", TTL,        s"multicast ttl [$TTL]")
    , OPT("-c", copies,       s"<int> copies [$copies]")
    , OPT("-b", bufferSize,   s"<int> buffer size [$bufferSize]")
    , OPT("-K",  keepAlive,   s"set KEEPALIVE on connections [$keepAlive]")
    , OPT("-json",  json,     s"Use json pickle rep for displayables, not binary [$json]")
    , OPT("-crlf",  crlf,     s"(string rep -- bytes; \\r\\n) [$crlf]")
    , OPT("-vpkt",  vpkt,     s"(string rep -- VARINT; bytes) [$vpkt]")
    , OPT("-ipkt",  ipkt,     s"(string rep -- INT; bytes) [$ipkt]")
    , OPT("-utf",   utf,      s"(string rep -- UTF data stream) [$utf]")
    , OPT("-nul",   nul,      s"(string rep -- bytes; \\u0000) [$nul]")
    , OPT("-k",     constant, s"send constant values for strings/displayables [$constant]")
    , OPT("-sync",  sync,     s"use synchronous channel (one extra process per channel) [$sync]")
    , OPT("-idle",  httpidle, s"<int> set http connection idle time (in seconds) [$httpidle]")
    , OPT("-S",  SND,         s"<int> network send buffer size (small tests message reassembly / separate packets) [$SND]")
    , OPT("-R",  RCV,         s"<int> network receive buffer size (small tests message reassembly / separate packets) [$RCV]")
    , OPT("-sd",  delay,      s"<int> delay after each simulated send of a line to the keyboard [$delay]")
    , OPT("-d", debugging,    s"<int> port on which to start the debugger [$debugging]")
    , OPT("-#",  copies,      s"<int> copies [$copies]")
    , OPT("-L=.+",            l => {level=l.substring(3)},      s"-L=<level> set level for subsequent -L logs [$level]")
    , OPT("-L",               m => LOGGING.setLevel(m, level),  s"<name> set loglevel of Log named <name> [$level]")
    , OPT("-L.+=.+",          l => l match {
        case s"-L$name=$lev" =>
             level = lev
             LOGGING.setLevel(name, level)
        },                    s"-L<name>=<level> set level of Log named <name> to <level>")
    , OPT("-A=.*",            l => {level=l.substring(3); LOGGING.setLevel("ALL", level)}, s"-A=<level> set default log level for all Logs to <level>")
    , ELSE("<anything else>", l => nonSwitch.enqueue(l), "Add this to the list of transmissions" )
    )

    def forwardNonSwitch(out: !![String]): PROC = proc ("Forward-nonSwitch"){
        repeat (nonSwitch.nonEmpty) { out!nonSwitch.dequeue(); sleep(delay) }
        out.closeOut()
    }

    def now: Nanoseconds = System.nanoTime

    def address(host: String, port: Int): InetSocketAddress = new java.net.InetSocketAddress(host, port)
    def address(host: String): InetAddress                  = java.net.InetAddress.getByName(host)
    def address(port: Int): InetSocketAddress               = new java.net.InetSocketAddress(port)

    //def OneOneBuf[T](size: Int) = new ox.cso.channels.BufChan[T](size)()()

  def MAIN: Unit

  def Main(): Unit =
  {
    if (debugging > 0) System.setProperty("io.threadcso.debugger.port", debugging.toString)
    if (debugging >= 0) Console.println(debugger)
    MAIN
  }

}

import Factories._

object bufferedStrings extends ManualTest("bufferedStrings -- round trips strings with BufferedSyncNetChannel") {

  import io.threadcso.component._

  import scala.collection.mutable.Queue

  val channel =  if (ipkt) BufferedSyncNetChannel.connected(address(host, port), IntPacketOutFactory, IntPacketInFactory)
                 else
                 if (vpkt) BufferedSyncNetChannel.connected(address(host, port), VarIntPacketOutFactory, VarIntPacketInFactory)
                 else
                 if (utf) BufferedSyncNetChannel.connected(address(host, port), UTF8OutFactory, UTF8InFactory)
                 else
                    BufferedSyncNetChannel.connected(address(host, port), crlfOutFactory, crlfInFactory)

  def MAIN: Unit = {
    val kbd = OneOne[String]("kbd")
    val fromHost = OneOneBuf[String](50, name="fromHost") // A synchronized channel causes deadlock under load
    val toHost   = OneOneBuf[String](50, name="toHost") // A synchronized channel causes deadlock under load
    val collected = OneOne[mutable.Queue[String]]("Collected")
    var n = copies
    val log = Log("MAIN")

    def prompt = Console.println("> <copies> %d | <text>".format(n))
    // Bootstrap the channel processes
    channel.CopyToNet(toHost).withExecutor(VIRTUAL).fork
    channel.CopyFromNet(fromHost).withExecutor(VIRTUAL).fork
    run(proc("ui") {
      repeat {
        prompt
        kbd ? () match {
          case cmd =>
            if (cmd.matches("[0-9]+")) {
              n = cmd.toInt
            }
            else {
              val sent = new mutable.Queue[String]
              var text = cmd
              val stride = 1 max (n / 10)
              for (i <- 0 until n) {
                if ((i % stride) == 0 && !constant) text = "((%s))".format(text)
                val message = "%d %s".format(i, text)
                toHost ! message
                sent.enqueue(message)
                if (i % stride == 0) Console.print("!%d ".format(i))
              }
              toHost ! "RTT"
              Console.println()
              val received = collected ? ()
              log.finest(s"collected=$collected")
              var k = 0
              while (received.nonEmpty) {
                val rx = received.dequeue()
                val sx = sent.dequeue()
                if (rx == sx) {
                  if (k % stride == 0) Console.print("=%d".format(k))
                }
                else
                  Console.println("Record %d: sx=%s rx=%s".format(k, sx, rx))
                k += 1
              }
              Console.println()
              received.clear()
              sent.clear()
              prompt
            }
        }
      }
      toHost.close()
    }
      || (if (nonSwitch.isEmpty) keyboard(kbd) else forwardNonSwitch(kbd)).withName("kbd")
      || proc("fromHost") {
      var start = now
      val results = new mutable.Queue[String]
      repeat {
        log.finest("fromHost?()")
        val decoded = fromHost ? ()
        log.finest(s"fromHost()=$decoded")
        decoded match {
          case "RTT" =>
            Console.println("RTT %g ns/message".format((now - start).toDouble / n.toDouble))
            collected ! results
            start = now
            ()
          case s =>
            log.finest(s"results.enqueue: $decoded")
            results.enqueue(s)
            ()
        }
      }
      toHost.close()
      kbd.close()
      collected.close()
    }
    )
    kbd.close()
    fromHost.close()
    exit()
  }
}

object buffered extends ManualTest("bufferedStrings -- round trips keyboard using BufferedSyncNetChannel") {
  import io.threadcso.component._

  val channel =  BufferedSyncNetChannel.connected(address(host, port), crlfOutFactory, crlfInFactory)

  def MAIN: Unit = {
    val kbd = OneOne[String]("kbd")
    val fromHost = OneOneBuf[String](50, name="fromHost") // A synchronized channel causes deadlock under load
    val toHost   = OneOneBuf[String](50, name="toHost") // A synchronized channel causes deadlock under load
    val log = Log("MAIN")

    // Bootstrap the channel processes
    channel.CopyToNet(toHost).withExecutor(VIRTUAL).fork
    channel.CopyFromNet(fromHost).withExecutor(VIRTUAL).fork

    run(
         keyboard(kbd)
      || proc("ui") {
         repeat {
          kbd ? () match {
            case "" =>
              kbd.close()
            case line =>
              log.fine(s"toHost ! $line")
              toHost ! line
          }
        }
        toHost.close()
      }
      || proc("fromHost") {
           var n = 0
           repeat  {
             n += 1
             val decoded = fromHost ? ()
             println(f"$n%-3d $decoded")
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


/**
        This program round-trips large numbers of strings using the
        current string-encoding. It checks that the received strings are
        identical to those that were sent.
*/
object strings extends ManualTest("strings -- round-trips encoded strings")  {
  import io.threadcso.component._

 import scala.collection.mutable.Queue

  def MAIN : Unit =
  {
    val channel =
                  if (sync)
                    NetChannel.connected(address(host, port))
                  else
                    SyncNetChannel.connected(address(host, port))

    channel.setOption(SO_RCVBUF, RCV)
    channel.setOption(SO_SNDBUF, SND)
    channel.setOption(TCP_NODELAY, nodelay)
    channel.setOption(SO_KEEPALIVE, keepAlive)

    val kbd              = OneOne[String]
    val fromHost, toHost = OneOneBuf[String](50) // A synchronized channel causes deadlock under load
    val collected        = OneOne[mutable.Queue[String]]
    var n = copies
    def prompt = Console.println("> <copies> %d | <text>".format(n))
    // Bootstrap the channel processes
    channel.CopyToNet(toHost, stringCodec.Encoder(bufferSize))
    channel.CopyFromNet(fromHost, stringCodec.Decoder(bufferSize))
    run (  proc ("ui") {
              prompt
              repeat { kbd ? () match {
                         case cmd =>
                           if (cmd.matches("[0-9]+"))
                           { n = cmd.toInt
                             prompt
                           }
                           else
                           { val sent = new mutable.Queue[String]
                             var text = cmd
                             val stride = n/10
                             for (i<-0 until n)
                             { if ((i % stride) == 0 && !constant) text = "((%s))".format(text)
                               val message = "%d %s".format(i, text)
                               toHost!message
                               sent.enqueue(message)
                               if (i % stride == 0) Console.print("!%d ".format(i))
                             }
                             toHost!""
                             Console.println()
                             val received = collected?()
                             var k=0
                             while (received.nonEmpty)
                             { val rx = received.dequeue()
                               val sx = sent.dequeue()
                               if (rx==sx)
                               { if (k % stride == 0) Console.print("=%d".format(k))
                               }
                               else
                                 Console.println("Record %d: sx=%s rx=%s".format(k,sx,rx))
                               k+=1
                             }
                             Console.println()
                             received.clear()
                             sent.clear()
                             prompt
                           }
                      }
                    }
              toHost.close()
           }
        || (if (nonSwitch.isEmpty) keyboard(kbd) else forwardNonSwitch(kbd)).withName("kbd")
        || proc ("fromHost") {
             var start = now
             val results = new mutable.Queue[String]
             repeat {
               fromHost ? () match {
                             case "" =>
                                Console.println("RTT %g ns/message".format((now-start).toDouble / n.toDouble))
                                collected!results
                                start=now
                                ()
                             case s  =>
                                results.enqueue(s)
                                ()
                          }
             }
             toHost.close()
             kbd.close()
             collected.close()
           }
        )
    kbd.close()
    fromHost.close()
    exit()
  }
}

/** Reflect uninterpreted bytes */
object echoserver extends ManualTest("echo -- (server) reflects uninterpreted bytes") {
  val log = new Log("echo")
  def MAIN : Unit =
  { // delay = 5000

    val server = new ServerSocket(port, listenSize, newSession)
    server.accept
    exit()
    System.exit(0)
  }

  def newSession(channel: NetChannel): Unit =
  {
    channel.setOption(SO_RCVBUF, RCV)
    channel.setOption(SO_SNDBUF, SND)
    channel.setOption(TCP_NODELAY, nodelay)
    val session =
        proc ("Session (%s)".format(channel)) {
            val inBuf, outBuf = ByteBuffer.allocateDirect(bufferSize)
            repeat {
              val count = channel.read(inBuf)
              inBuf.flip
              log.fine("Read %d bytes".format(inBuf.remaining))
              channel.writeAll(inBuf)
              inBuf.clear
              ()
            }
            log.info("Closed")
            channel.close()
          }

    val handle = session.fork
    // handle on the session: one day we may use it
  }
}

object datagramecho extends ManualTest("datagramecho -- (server) reflects uninterpreted byte datagrams") {
  val log = new Log("datagramecho")
  def MAIN : Unit =
  { val channel = NetDatagramChannel.bound(address(host, port))
    Console.println(channel.getLocalAddress)
    channel.setOption(SO_RCVBUF, RCV)
    channel.setOption(SO_SNDBUF, SND)
    channel.setOption(SO_REUSEADDR, value = true)
    val session =
        proc ("Session (%s)".format(channel)) {
            val inBuf = ByteBuffer.allocateDirect(bufferSize)
            repeat {
              val from = channel.receive(inBuf)
              log.fine("Read %d bytes from %s".format(inBuf.remaining, from))
              log.fine("Sending %s".format(inBuf))
              attempt { channel.send(inBuf, from); () }
                      {
                        log.fine("Problem: %s".format(channel.lastException))
                      }
              log.fine("Sent %s".format(inBuf))
              ()
            }
          }

    val handle = session.fork
    // handle on the session: one day we may use it
  }
}

/**
        Keyboard and pseudo-keyboard messages forwarded in the current string encoding (as datagrams) from terminal to host:port, and vice-versa
*/
object datagrams extends ManualTest("datagrams -- (client) messages forwarded (as datagrams) from terminal to host:port, and vice-versa") {
  import io.threadcso.component._
 import ox.eieio.options._

  def MAIN : Unit =
  { val log = new Log("datagrams")
    val channel: DatagramConnector = NetDatagramChannel.connected(address(host, port))
    channel.setOption(SO_RCVBUF, RCV)
    channel.setOption(SO_SNDBUF, SND)
    if (TTL>0) channel.setOption(IP_MULTICAST_TTL, TTL)


    val kbd      = OneOne[String]
    val fromHost = OneOne[(String, java.net.SocketAddress)]
    channel.DatagramsFromNet(fromHost, stringCodec.Decoder(bufferSize))
    channel.CopyToNet(kbd, stringCodec.Encoder(bufferSize))

    run (  (if (nonSwitch.isEmpty) keyboard(kbd) else forwardNonSwitch(kbd)).withName("kbd")
        || proc ("fromHost") {
             repeat {
               fromHost ? { case (s: String, addr: java.net.SocketAddress) => println(s"$addr: $s") }
             }
             log.fine(s"fromHost: channel closed $fromHost")
           }
        )
    kbd.close()
    fromHost.close()
    exit()
  }
}

/**
        Listen to multicast datagrams encoded with the specified string codec
*/
object listen extends ManualTest("listen -- listen to multicast datagrams") {
  import ox.eieio.options._

  def MAIN : Unit =
  { val log = new Log("datagrams")
    val channel: MulticastConnector = NetDatagramChannel.multicast(address(port), address(host, port).getAddress)
    channel.setOption(SO_RCVBUF, RCV)
    channel.setOption(SO_SNDBUF, SND)


    val kbd      = OneOne[String]
    val fromHost = OneOne[(String, java.net.SocketAddress)]
    val key = channel.join(address(multicast))
    // Make me join some more ...
    Console.println(key)
    Console.println(s"Enable multicasting by: sudo route -nv add -net ${address(multicast)} -interface ${channel.getNI.getName}")
    log.finest(s"listening on: $channel")
    channel.DatagramsFromNet(fromHost, stringCodec.Decoder(bufferSize))

    run (  proc ("fromHost") {
             repeat {
               fromHost ? () match { case s => Console.println(s) }
             }
           }
        )
    kbd.close()
    fromHost.close()
    exit()
    ()
  }
}

/**
        Listen to datagrams encoded with the specified string codec.
        Datagrams are not connection-oriented, hence the `SocketAddress`
        components of messages to and from the channel.
*/
object bound extends ManualTest("bound -- listen to datagrams") {
  import ox.eieio.options._

  def MAIN : Unit =
  { val log = new Log("datagrams")

    val channel = NetDatagramChannel.bound(address(host, port))
    channel.setOption(SO_RCVBUF, RCV)
    channel.setOption(SO_SNDBUF, SND)


    val reflect  = OneOne[(String, java.net.SocketAddress)]
    val fromHost = OneOne[(String, java.net.SocketAddress)]
    channel.DatagramsFromNet(fromHost, stringCodec.Decoder(bufferSize))
    channel.DatagramsToNet(reflect, stringCodec.Encoder(bufferSize))
    Console.println(channel)

    run (  proc ("fromHost") {
             repeat {
               fromHost ? () match
               { case (s, sender) =>
                      Console.println(s"$sender sent $s")
                      reflect!(s"bound reflected $s", sender)
               }
             }
           }
        )
    reflect.close()
    fromHost.close()
    exit()
    ()
  }
}

/**
        Send multicast datagrams encoded with the specified string codec
*/
object speak extends ManualTest("speak -- (client) messages forwarded (as datagrams) from terminal to the given multicast address") {
  import io.threadcso.component._
  import ox.eieio.options._
  def MAIN : Unit =
  { val log = new Log("speak")
    val channel: DatagramConnector = NetDatagramChannel.connected(address(multicast, port))
    channel.setOption(SO_RCVBUF, RCV)
    channel.setOption(SO_SNDBUF, SND)


    val kbd      = OneOne[String]
    val fromHost = OneOne[(String, java.net.SocketAddress)]
    channel.DatagramsFromNet(fromHost, stringCodec.Decoder(bufferSize))
    channel.CopyToNet(kbd, stringCodec.Encoder(bufferSize))
    log.info(s"Datagram connector: $channel")
    run (  (if (nonSwitch.isEmpty) keyboard(kbd) else forwardNonSwitch(kbd)).withName("kbd")
        || proc ("fromHost") {
             repeat {
               fromHost ? () match { case s => Console.println(s) }
             }
             log.info("fromHost: fromHost $channel closed")
           }
        )
    kbd.close()
    fromHost.close()
    exit()
  }
}

/**
        An http server that reflects the headers of requests sent to it.
*/
object http extends ManualTest("http -- (server) pretends to be an http server")  {
  val log = new Log("http")
  
  def MAIN : Unit =
  run( π ("Runtime") { } || 
       π { 
         val server = new ServerSocket(port, listenSize, newSession)
         log.info("Server accepting on -p "+port)
         server.accept
         exit()
         System.exit(0)
         })

  var count = 0

  /** Buf bound for the network channels: 0=unbounded */
  val BOUND = 0

  def newSession(channel: NetChannel): Unit =
  { channel.setOption(SO_RCVBUF, RCV)
    channel.setOption(SO_SNDBUF, SND)
    channel.setOption(TCP_NODELAY, nodelay)
    val serverDate = new java.util.Date().toString
    val fromNet    = OneOneBuf[String](BOUND)
    val toNet      = OneOneBuf[String](BOUND)
    val clientnum  = { count+=1; count }
    log.info("New client %d".format(clientnum))
    
    @inline def chunk(line: String): Unit =
    { send("%x".format(line.length))
      log.finest(s"http: '$line''")
      send(line)
    }
    
    @inline def send(line: String): Unit = {
      toNet!line
    }

    /** Whether to use the time-bounded `alt` or the `readBefore`
      * when reading `fromNet`.
      *
      * TODO: understand why when BOUND was 10, the `readBefore` route
      *       deadlocked; yet the `alt` route didn't.
      *       It's dangerous to have an unbounded `fromNet`, but what
      *       should the `BOUND` be? (100 worked on my tests, ....)
      */
    val readBefore: Boolean = true

    val service =
        (  π { channel.CopyFromNet(fromNet, CRLFString.Decoder(bufferSize)) }
        || π { channel.CopyToNet(toNet, CRLFString.Encoder(bufferSize)) }
        || proc ("listener") {
            val header = new scala.collection.mutable.Queue[String]
            def processRequestLine(request: String): Unit = {
                log.fine("Client %d: '%s'".format(clientnum, request))
                val line = request.trim
                log.finest(s"TRIMMED: $line")
                if (line == "")
                // end of the header reached
                {
                  send("HTTP/1.1 200 OK")
                  send("Date: " + serverDate)
                  send("Content-Type: text/html")
                  //send("Connection: close")
                  send("Transfer-Encoding: chunked")
                  send("expect: 100-continue")
                  send("")
                  chunk("<!DOCTYPE html>")
                  chunk("<head><title>Header Session %d</title></head>".format(clientnum))
                  chunk("<body>")
                  chunk(s"<h2>Header for session %d (${new java.util.Date().toString})</h2>".format(clientnum))
                  chunk("<ul>")
                  while (header.nonEmpty) chunk(s"<li><tt>${header.dequeue()}</tt></li>")
                  chunk("</ul>")
                  chunk("</body>")
                  chunk("</html>")
                  chunk("")
                }
                else
                // add another line to the header
                {
                  header.enqueue(line)
                }
            }
            // Decode and forward lines from the client
            var running = true
            if (readBefore) {
              while (running)
                    fromNet.readBefore(seconds(httpidle)) match {
                      case Some(request) => processRequestLine(request)
                      case None          => log.fine(s"Client $clientnum went idle"); running = false
                    }
            } else
            while (running)
              alt {( fromNet =?=> processRequestLine
                   | after(seconds(httpidle)) ==> {
                          log.fine(s"Client $clientnum went idle")
                          running = false
                     }
                  )}
       
            log.info("client %d: <closing channel>".format(clientnum))
            // TODO: there is some confusion about exactly how to close down a channel of this kind
            // It's enough to close down the transfer channels           
            toNet.close()
            fromNet.close()
            //channel.shutdownInput  // REDUNDANT -- fromNet.close does this
            //channel.shutdownOutput // REDUNDANT -- toNet.close does this
            //channel.close          // REDUNDANT -- nothing more to do when both are closed; and can cause an asyncClose exception
          }
       )
       
    val handler = 
    fork ( π { log.info("serving")
               service()
               log.info("serve %d: terminated".format(clientnum)) 
             } 
         )
    log.info("Handler spawned")                    
  } 
}












































