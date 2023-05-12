package ox.net

import io.threadcso.{PROC, proc, repeat}

import java.io.FileInputStream
import java.net.ServerSocket
import java.security.KeyStore

object SSLChannel extends ox.logging.Log("SSL") {

  import javax.net._
  import javax.net.ssl._

  trait Credential {}
  case class  TLSCredential(passPhrase: String, keyStoreFile: java.io.File) extends Credential
  case object TCPCredential extends Credential

  def serverSocketFactory(credentials: Credential): ServerSocketFactory = {
    credentials match {
      case TCPCredential => ServerSocketFactory.getDefault()
      case TLSCredential(passPhrase, keyStoreFile) =>
        val context = SSLContext.getInstance("TLSv1.2")
        val keyStore = KeyStore.getInstance("JKS")
        val keyManagers = KeyManagerFactory.getInstance("SunX509")
        keyStore.load(new FileInputStream(keyStoreFile), passPhrase.toCharArray)
        keyManagers.init(keyStore, passPhrase.toCharArray)
        finest(s"${keyManagers.getKeyManagers()}")
        context.init(keyManagers.getKeyManagers(), null, null)
        context.getServerSocketFactory()
    }
  }

  private def newServerSocket(credential: Credential, port: Int): ServerSocket = {
    val factory = serverSocketFactory(credential)
    factory.createServerSocket(port)
  }

  /**
    * Build a `TypedSSLChannel[OUT,IN]` that connects to `//host:port` using the given credential.
    * If the credential is a `TLSCredential` then communication uses `SSL/TLS`, otherwise a plain
    * `TCP` channel is used.
    *
    * The details of a `TLSCredential` are ignored they may be eventually used to provide
    * the client's individual credential to the server.
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
      case _: TLSCredential => SSLSocketFactory.getDefault()
      case TCPCredential    => javax.net.SocketFactory.getDefault()
    }
    val socket        = socketFactory.createSocket(host, port)
    val channel       = factory.newChannel(socket)
    fine(s"Client socket $socket")
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
                      factory:     ox.net.TypedChannelFactory[OUT, IN],
                      session:     TypedSSLChannel[OUT, IN] => Unit,
                      sync:        Boolean = true
                     ): PROC = proc(s"Server $credential $port (sync=$sync)") {
      val socket: ServerSocket = newServerSocket(credential, port)
      val sync = true //
      info(s"Serving on $port with $credential")
      repeat {
        val client = socket.accept
        client.setTcpNoDelay(sync)
        val channel = factory.newChannel(client)
        session(channel)
      }
    }
}

