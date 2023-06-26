package io.threadcso.net

import io.threadcso.PROC
import io.threadcso.net.SSLTransport.Credential
import io.threadcso.net.transport.{
  Options,
  NetConnection,
  TypedTransportFactory,
  TypedSSLTransport
}

/** A factory for client `NetConnection`s using SSL/TLS as transport, and for
  * servers whose sessions use such `BNetConnection`s.
  *
  * @see ox.net.SSLTransport
  */
object SSLConnection {
  /**
    * Returns a server `PROC`ess that (when run or forked) listens to
    * the given `port`; offering `SSL/TLS`-secured transport that
    * uses the given `credential`. Each invocation of its `session`
    * function is passed a secure `NetConnection[OUT, IN]` corresponding
    * to the secure transport used by the client that caused the
    * invocation.
    * @see NetConnection
    */
  def server[OUT, IN](
                       credential: Credential,
                       port: Int,
                       factory: TypedTransportFactory[OUT, IN],
                       name: => String = ""
  )(session: NetConnection[OUT, IN] => Unit): PROC = {
    val ocs = Options.outChanSize
    val ics = Options.inChanSize
    SSLTransport.server(credential, port, factory) {
      case sslChannel: TypedSSLTransport[OUT, IN] =>
        val connection =
          Options.withOptions(outChanSize = ocs, inChanSize = ics) {
            transport.NetConnection[OUT, IN](sslChannel, name)
          }
        session(connection)
    }
  }

  /**
    * Return a secure `NetConnection[OUT, IN]` to the SSL/TLS server
    * at `host:port`, supplying the given credential.
    * This can be `TLSWithoutCredential` if the server does not require
    * a credential from the client.
    * @see NetConnection
    */
  def client[OUT, IN](
                       credential: Credential,
                       host: String,
                       port: Int,
                       factory: TypedTransportFactory[OUT, IN],
                       name: => String = ""
  ): NetConnection[OUT, IN] = {
    val channel = SSLTransport.client(credential, host, port, factory)
    transport.NetConnection(channel, name)
  }
}
