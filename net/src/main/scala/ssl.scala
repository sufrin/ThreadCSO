package ox.net

/**
  * An experimental ssl setup without dependencies.
  *
  * Intended to explore my understanding of SSL, and
  * to provide the basis for generalising the existing eieio
  * (NetChannel) framework. These are quick-and-dirty
  * experiments, and I don't recommend using them.
  *
  * Results: client and server connections all but 1 work. (Later: ALL WORK)
  * Tested when necessary by using external hosts; secure or insecure.
  *
  * EXCEPT: that I cannot establish a secure server socket. The
  * problem is something to do with the server certificate.
  * It may well be connected with my understanding of how to generate
  * a local certificate for Java. But I have run out of time to investigate
  * this (this season).
  *
  * LATER: it /was/ connected with server certificates on my development
  * machine. The topic of (installing) self-signed server certificates is exasperatingly
  * badly documented, and the bulk of the documentation serves to obscure the
  * essentials.
  */

import io.threadcso.component.keyboard
import ox.logging._

import java.io.{File, FileInputStream}
import java.net.{ServerSocket, Socket}
import java.security.KeyStore


object ssl extends ox.logging.Log("ssl") {
  val log = this

  import javax.net._
  import javax.net.ssl._

  trait ServerSocketSpec {}
  case class  TLSSocket (passPhrase: String, keyStoreFile: java.io.File) extends ServerSocketSpec
  case object NetSocket extends ServerSocketSpec

  def serverSocketFactory(spec: ServerSocketSpec): ServerSocketFactory = {
    spec match {
      case NetSocket => ServerSocketFactory.getDefault()
      case TLSSocket(passPhrase, keyStoreFile) =>
        val context = SSLContext.getInstance("TLSv1.2")
        val keyStore = KeyStore.getInstance("JKS")
        val keyManagers = KeyManagerFactory.getInstance("SunX509")
        keyStore.load(new FileInputStream(keyStoreFile), passPhrase.toCharArray)
        keyManagers.init(keyStore, passPhrase.toCharArray)
        log.finest(s"${keyManagers.getKeyManagers()}")
        context.init(keyManagers.getKeyManagers(), null, null)
        context.getServerSocketFactory()
    }
  }

  def serverSocket(spec: ServerSocketSpec, port: Int): ServerSocket = {
    val factory = serverSocketFactory(spec)
    factory.createServerSocket(port)
  }

  def clientSocket(host: String, port: Int, secure: Boolean): Socket = {
    val factory = if (secure) SSLSocketFactory.getDefault() else javax.net.SocketFactory.getDefault()
    val socket = factory.createSocket(host, port)
    log.fine(s"Client socket $socket")
    socket
  }

  import io.threadcso._
  case class ServerConnection[Request, Response](toServer: !![Request], fromServer: ??[Response])
  case class ClientConnection[Request,Response](fromClient: ??[Request], toClient: !![Response])

  type Request = String
  type Response = String

  def connection(host: String, port: Int, secure: Boolean): ServerConnection[Request,Response] = {
    val sync = true
    val socket = clientSocket(host, port, secure)
    log.finest(s"Client Socket: $socket")
    if (secure) {
      socket.asInstanceOf[SSLSocket].startHandshake()
    }
    else socket.setTcpNoDelay(sync)
    val requests = OneOne[Request]
    val responses = OneOne[Response]
    val in = PseudoChannel(socket.getInputStream)
    val out = PseudoChannel(socket.getOutputStream)
    log.fine(s"Connection $socket ==> ${socket.getChannel}")
    in.CopyFromNet(responses, ox.eieio.codecs.CRLFString.Decoder(32767)).fork
    out.CopyToNet(requests, ox.eieio.codecs.CRLFString.Encoder(32767)).fork
    ServerConnection(requests, responses)
  }

  def server(socket: ServerSocket, handle: ClientConnection[Request,Response]=>Unit): PROC = proc (s"Server $socket") {
    { val sync = true //
      repeat {
        val client = socket.accept
        client.setTcpNoDelay(sync)
        val requests  = OneOne[Request]
        val responses = OneOne[Response]
        val in = PseudoChannel(client.getInputStream)
        val out = PseudoChannel(client.getOutputStream)
        in.CopyFromNet(requests, ox.eieio.codecs.CRLFString.Decoder(32767)).fork
        out.CopyToNet(responses, ox.eieio.codecs.CRLFString.Encoder(32767)).fork
        handle(ClientConnection(requests, responses))
      }
    }
  }

}

import app.OPT._
import io.threadcso._

object test extends App {
  var port: Int       = 3471
  var host: String    = "localhost"
  var httpidle: Int   = 5 //seconds
  var secure: Boolean = false
  var client: Boolean = false
  var level: String   = "ALL"
  var http: Boolean = false

  val log = ox.logging.Log("test")

  override val Options: List[Opt] = List (
       OPT("-p", port, "<port>")
    ,  OPT("-i", httpidle, "http client idle time")
    ,  OPT("-h", host, "<host>")
    ,  OPT("-t", secure, "use TLS")
    ,  OPT("-c", { client=true}, "set client mode")
    ,  OPT("-s", { client=false}, "set server mode")
    ,  OPT("-sec", { secure=true}, "set secure mode")
    ,  OPT("-http", { http=true}, "set http mode")
    , OPT("-L=.+", l => {
      level = l.substring(3)
    }, s"-L=<level> set level for subsequent -L logs [$level]")
    , OPT("-L", m => ox.logging.Logging.setLevel(m, level), s"<name> set loglevel of Log named <name> [$level]")
    , OPT("-L.+=.+", l => l match {
      case s"-L$name=$lev" =>
        level = lev
        ox.logging.Logging.setLevel(name, level)
    }, s"-L<name>=<level> set level of Log named <name> to <level>")
    , OPT("-A=.*", l => {
      level = l.substring(3); ox.logging.Logging.setLevel("ALL", level)
    }, s"-A=<level> set default log level for all Logs to <level>")
    )

  override val Command: String = "ox.net.test"

  override def Main(): Unit = {
    println(debugger)
    if (client) testClient() else testServer()
  }

  import ox.net.ssl._
  def testClient(): Unit =
    if (http) testHTTPClient() else testForwardClient()

  def testHTTPClient(): Unit = {
    ssl.info(s"HTTP request to $host:$port")
    val ServerConnection(to, from) = connection(host, port, secure)
    ssl.info(s"To $to; from $from ")
    val kbd = OneOne[String]
    val client = proc("client") {
      ssl.fine("Starting client")
      to!"GET / HTTP/1.1"
      to!""
    }
    val echo = proc("echo") {
      ssl.fine("Starting echo")
      repeat {
        from ? { line => println(s"'$line'") }
      }
    }

    (client || echo)()
  }

  /** Forward from the keyboard to the host */
  def testForwardClient(): Unit = {
    ssl.info(s"Connecting to $host:$port")
    val ServerConnection(to, from) = connection(host, port, secure)
    ssl.info(s"To $to; from $from ")
    val kbd = OneOne[String]
    val client = proc ("client") {
      ssl.fine("Starting client")
      repeat {
        kbd ? { line => ssl.log.fine(s"!$line"); to!line }
      }
    }
    val echo = proc("echo") {
      ssl.fine("Starting echo")
      repeat {
        from ? { line => println(s"'$line'") }
      }
    }

    (client || keyboard(kbd) || echo)()
  }

  def testServer(): Unit = {
    ssl.info(s"Serving $host:$port")
    val socketSpec = if (secure) TLSSocket("xyzzyxyzzy", new File("/Users/sufrin/.keystore")) else NetSocket
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
    val readBefore: Boolean = true

    val listener = proc("listener") {
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
              case None => log.fine(s"Client $clientnum went idle"); running = false
            }
        } else
          while (running)
            alt {
              (fromNet =?=> processRequestLine
                | after(seconds(httpidle)) ==> {
                        log.fine(s"Client $clientnum went idle")
                        running = false
                  }
                )
            }

        log.info("client %d: <closing channel>".format(clientnum))
        // TODO: there is some confusion about exactly how to close down a channel of this kind
        // It's enough to close down the transfer channels
        toNet.closeOut()
        fromNet.closeIn()
      }


    val handler =
      fork(π {
        log.info("serving")
        listener()
        log.info("serve %d: terminated".format(clientnum))
      })

    log.info("Handler spawned")
  }

}