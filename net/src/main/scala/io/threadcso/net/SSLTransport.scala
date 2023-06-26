package io.threadcso.net

import io.threadcso.net.transport.{Options, TypedTransportFactory, TypedSSLTransport}
import io.threadcso.{PROC, proc, repeat}

import java.io.FileInputStream
import java.net.{ServerSocket, Socket}
import java.security.KeyStore

/**
  *  The factory for `TypedSSLTransport`s as well as servers using `SSL/TLS` transport.
  */
object SSLTransport  {
  val log = new ox.logging.Log()

  import javax.net._
  import javax.net.ssl._

  /**
    * Representation of a client or server credential for use
    * with SSL/TLS or with TCP.
    */
  trait Credential {}
  /**
    * Server (or client) credential: the `passphrase` is for a server certificate stored in the
    * given `keyStoreFile`.
    *
    * Server transport are built using SSL/TLS.
    * Client transport are built using client-authenticated TLS.
    *
    */
  case class  TLSCredential(passPhrase: String, keyStoreFile: java.io.File) extends Credential

  /**
    * No credential is supplied: both client and server transport
    * are built using TCP.
    */
  case object  TLSWithoutCredential extends Credential

  /**
    * No credential is supplied: both client and server transport
    * are built using TCP.
    */
  case object TCPCredential extends Credential

  /**
    * Return a server socket factory using the given `credential`. If the credential
    * is a `TCPCredential` the server socket factory returned is the default `javax.netchannels.ServerDocketFactory`.
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
      case TCPCredential        => ServerSocketFactory.getDefault()
      case TLSWithoutCredential => ServerSocketFactory.getDefault()
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
      case TLSWithoutCredential => SocketFactory.getDefault()
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
    * Build a `TypedSSLTransport[OUT,IN]` that connects to `//host:port` using the given credential.
    *
    * If the credential is a `TLSWithoutCredential` (or a `TCPCredential`) then communication uses `SSL/TLS`, but
    * the client identity is not certified.
    *
    * If the credential is a `TLSCredential` then communication uses `SSL/TLS`, using the client identity
    * as certified. otherwise a plain `TCP` channel is used.
    *
    * @param factory defines the codecs used for the `TypedSSLTransport`
    */
  def client[OUT, IN](credential: Credential, host: String, port: Int, factory: TypedTransportFactory[OUT, IN]): TypedSSLTransport[OUT,IN] = {
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
    val channel       = factory.newTransport(socket)
    if (log.logging) log.fine(s"Client socket $socket")
    channel
  }

  /**
    *  Construct and start a server socket bound to `port`. The socket
    *  uses the `SSL/TLS` transport protocol if credential is
    *  {{{
    *    TLSCredential(passPhrase: String, keyStoreFile: java.io.File)
    *  }}}
    *
    * and, in this case, if `Options.clientAuth` is true at the point of its construction,
    * then the server will negotiate client authentication as connections
    * are made.
    *
    *  Otherwise the plain `TCP` protocol is used for transport.
    *
    *  When a client contacts the server, a `TypedSSLTransport` is constructed from the
    *  client socket by the given `factory` and passed to `session`. The latter is
    *  expected to fork a session to handle the client.
    *
    */
  def server[OUT, IN](credential:  Credential,
                      port:        Int,
                      factory:     TypedTransportFactory[OUT, IN])
                      (session:     TypedSSLTransport[OUT, IN] => Unit): PROC =
    proc(s"Server $credential $port") {
      val socket: ServerSocket = newServerSocket(credential, port)
      val clientAuth = Options.clientAuth
      if (clientAuth) socket match {
       case socket: SSLServerSocket => socket.setNeedClientAuth(true)
       case _ =>
      }
      val sync = Options.sync
      if (log.logging) log.fine(s"Serving on $port with $credential")
      repeat {
        val client = socket.accept
        client.setTcpNoDelay(sync)
        val channel = factory.newTransport(client)
        session(channel)
      }
    }
}




