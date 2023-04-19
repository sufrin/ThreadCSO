package ox.net
import ox.eieio.Logging._

import java.io.FileInputStream
import java.net.ServerSocket
import java.security.KeyStore

object ssl {
 
  import javax.net._
  import javax.net.ssl._

  trait ServerSocketSpec {}

  case class  TLSSocket (passPhrase: String, keyStoreFile: java.io.File) extends ServerSocketSpec
  case object NetSocket extends ServerSocketSpec

  def serverSocketFactory(spec: ServerSocketSpec): ServerSocketFactory = {
    spec match {
      case NetSocket => ServerSocketFactory.getDefault()
      case TLSSocket(passPhrase, keyStoreFile) =>
        val context = SSLContext.getInstance("TLS")
        val keyStore = KeyStore.getInstance("JKS")
        val keyManagers = KeyManagerFactory.getInstance("SunX509")
        keyStore.load(new FileInputStream(keyStoreFile), passPhrase.toCharArray)
        keyManagers.init(keyStore, passPhrase.toCharArray)
        context.init(keyManagers.getKeyManagers(), null, null)
        context.getServerSocketFactory()
    }
  }

  def serverSocket(spec: ServerSocketSpec, port: Int): ServerSocket = {
    val factory = serverSocketFactory(spec)
    factory.createServerSocket(port)
  }

}