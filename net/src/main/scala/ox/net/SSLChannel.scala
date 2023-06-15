package ox.net

import io.threadcso.{PROC, proc, repeat}
import ox.net.SSLChannel.Credential

import java.io.FileInputStream
import java.net.{ServerSocket, Socket}
import java.security.KeyStore

object SSLChannel  {
  val log = new ox.logging.Log()

  import javax.net._
  import javax.net.ssl._

  /**
    * Representation of a client or server credential for use
    * with SSL/TLS or with TCP.
    */
  trait Credential {}
  /**
    * Server credential: the `passphrase` is for a server certificate stored in the
    * given `keyStoreFile`.
    */
  case class  TLSCredential(passPhrase: String, keyStoreFile: java.io.File) extends Credential

  case object  TLSWithoutCredential extends Credential

  /**
    * A meaningless server credential for use with an unencrypted TCP channel.
    */
  case object TCPCredential extends Credential

  /**
    * Return a server socket factory using the given `credential`. If the credential
    * is a `TCPCredential` the server socket factory returned is the default `javax.net.ServerDocketFactory`.
    * If the credential is a `TLSCredential` then the socket factory returned is constructed using the method described
    * at some length in
    * {{{
    * https://docs.oracle.com/en/java/javase/11/security/sample-code-illustrating-secure-socket-connection-client-and-server.html
    * }}}
    *
    * @param credentials
    */
  def serverSocketFactory(credential: Credential): ServerSocketFactory = {
    credential match {
      case TCPCredential => ServerSocketFactory.getDefault()
      case TLSCredential(passPhrase, keyStoreFile) =>
        val context = SSLContext.getInstance("TLSv1.2")
        val keyStore = KeyStore.getInstance("JKS")
        val keyManagers = KeyManagerFactory.getInstance("SunX509")
        keyStore.load(new FileInputStream(keyStoreFile), passPhrase.toCharArray)
        keyManagers.init(keyStore, passPhrase.toCharArray)
        if (log.logging) log.fine(s"${keyManagers.getKeyManagers()}")
        context.init(keyManagers.getKeyManagers(), null, null)
        context.getServerSocketFactory()
    }
  }

  /**
    *
    * Return a socket factory using the given `credential`.
    *
    */
  def clientSocketFactory(credential: Credential): SocketFactory = {
    credential match {
      case TCPCredential => SocketFactory.getDefault()
      case TLSCredential(passPhrase, keyStoreFile) =>
        val context = SSLContext.getInstance("TLSv1.2")
        val keyStore = KeyStore.getInstance("JKS")
        val keyManagers = KeyManagerFactory.getInstance("SunX509")
        keyStore.load(new FileInputStream(keyStoreFile), passPhrase.toCharArray)
        keyManagers.init(keyStore, passPhrase.toCharArray)
        if (log.logging) log.finest(s"${keyManagers.getKeyManagers()}")
        context.init(keyManagers.getKeyManagers(), null, null)
        context.getSocketFactory()
    }
  }

  private def newServerSocket(credential: Credential, port: Int): ServerSocket = {
    val factory = serverSocketFactory(credential)
    factory.createServerSocket(port)
  }

  private def newSocket(credential: Credential, host: String, port: Int): Socket = {
    val factory = clientSocketFactory(credential)
    factory.createSocket(host, port)
  }

  /**
    * Build a `Typedlog[OUT,IN]` that connects to `//host:port` using the given credential.
    * If the credential is a `TLSWithoutCredential` then communication uses `SSL/TLS`, but
    * the client identity is not certified.
    * If the credential is a `TLSCredential`
    * then communication uses `SSL/TLS`, using the client identity
    * as certified. otherwise a plain `TCP` channel is used.
    *
    *
    *
    *
    * @param credential
    * @param host
    * @param port
    * @param factory constructs the codecs used for the `TypedSSLChannel`
    * @tparam OUT
    * @tparam IN
    * @return A typed socket whose `From`
    */
  def client[OUT, IN](credential: Credential, host: String, port: Int, factory: ox.net.TypedChannelFactory[OUT, IN]): TypedSSLChannel[OUT,IN] = {
    val socketFactory = credential match {
      case TLSWithoutCredential => SSLSocketFactory.getDefault()
      case _ : TLSCredential    => clientSocketFactory(credential)
      case TCPCredential        => javax.net.SocketFactory.getDefault()
    }
    val socket = socketFactory.createSocket(host, port)
    socket match {
      case socket: SSLSocket => socket.startHandshake()
      case _                 =>
    }
    val channel       = factory.newChannel(socket)
    if (log.logging) log.fine(s"Client socket $socket")
    channel
  }

  /**
    *  Construct and start a server socket bound to `port`. The socket
    *  uses the `SSL/TLS` protocol if credential is
    *  {{{
    *    TLSCredential(passPhrase: String, keyStoreFile: java.io.File)
    *  }}}
    *
    *  and otherwise uses the plain `TCP` protocol.
    *
    *  When a client contacts the server, a `TypedSSLChannel` is constructed from the
    *  client socket by the given `factory` and passed to `session`. The latter is
    *  expected to fork a session to handle the client.
    *
    */
  def server[OUT, IN](credential:  Credential,
                      port:        Int,
                      factory:     ox.net.TypedChannelFactory[OUT, IN])
                      (session:     TypedSSLChannel[OUT, IN] => Unit): PROC =
    proc(s"Server $credential $port") {
      val socket: ServerSocket = newServerSocket(credential, port)
      val clientAuth = ChannelOptions.clientAuth
      if (clientAuth) socket match {
       case socket: SSLServerSocket => socket.setNeedClientAuth(true)
       case _ =>
      }
      val sync = ChannelOptions.sync
      if (log.logging) log.fine(s"Serving on $port with $credential")
      repeat {
        val client = socket.accept
        client.setTcpNoDelay(sync)
        val channel = factory.newChannel(client)
        session(channel)
      }
    }
}

/** A factory for client connections and servers using SSL/TLS as transport. */
object SSLConnection {
  def server[OUT, IN](credential: Credential,
                      port: Int,
                      factory: ox.net.TypedChannelFactory[OUT, IN],
                      name: => String = "")
                      (session: NetConnection[OUT, IN] => Unit): PROC = {
    val ocs = ChannelOptions.outChanSize
    val ics = ChannelOptions.inChanSize
    SSLChannel.server(credential, port, factory) {
          case sslChannel: TypedSSLChannel[OUT, IN] =>
            val connection = ChannelOptions.withOptions(outChanSize = ocs, inChanSize = ics) {
                NetConnection[OUT, IN](sslChannel, name)
            }
            session(connection)
    }
  }

  def client[OUT, IN](credential: Credential, host: String, port: Int, factory: ox.net.TypedChannelFactory[OUT, IN], name: => String = ""): NetConnection[OUT,IN] = {
    val channel = SSLChannel.client(credential, host, port, factory)
    NetConnection(channel, name)
  }
}


