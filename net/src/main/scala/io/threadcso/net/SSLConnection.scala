package io.threadcso.net

import io.threadcso.PROC
import io.threadcso.net.SSLChannel.Credential
import io.threadcso.net.channels.{
  ChannelOptions,
  NetConnection,
  TypedChannelFactory,
  TypedSSLChannel
}

/** A factory for client connections and servers using SSL/TLS as transport. */
object SSLConnection {
  def server[OUT, IN](
      credential: Credential,
      port: Int,
      factory: TypedChannelFactory[OUT, IN],
      name: => String = ""
  )(session: NetConnection[OUT, IN] => Unit): PROC = {
    val ocs = ChannelOptions.outChanSize
    val ics = ChannelOptions.inChanSize
    SSLChannel.server(credential, port, factory) {
      case sslChannel: TypedSSLChannel[OUT, IN] =>
        val connection =
          ChannelOptions.withOptions(outChanSize = ocs, inChanSize = ics) {
            channels.NetConnection[OUT, IN](sslChannel, name)
          }
        session(connection)
    }
  }

  def client[OUT, IN](
      credential: Credential,
      host: String,
      port: Int,
      factory: TypedChannelFactory[OUT, IN],
      name: => String = ""
  ): NetConnection[OUT, IN] = {
    val channel = SSLChannel.client(credential, host, port, factory)
    channels.NetConnection(channel, name)
  }
}
