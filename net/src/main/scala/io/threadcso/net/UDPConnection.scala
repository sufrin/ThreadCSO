package io.threadcso.net

import io.threadcso.net.UDPChannel.UDP
import io.threadcso.net.channels.{
  NetConnection,
  TypedChannelFactory,
  TypedUDPChannel
}

import java.net.InetSocketAddress

object UDPConnection {

  /** A network connection bound to `address` that sends/receives `UDP[OUT]/UDP[IN]` */
  def bind[OUT, IN](
      address: InetSocketAddress,
      factory: TypedChannelFactory[OUT, IN],
      name: String = ""
  ): NetConnection[UDP[OUT], UDP[IN]] = {
    val channel: TypedUDPChannel[UDP[OUT], UDP[IN]] =
      UDPChannel.bind[OUT, IN](address, factory)
    channels.NetConnection(channel, name)
  }

  /** A network connection connected to `address` that sends/receives `UDP[OUT]/UDP[IN]` */
  def connect[OUT, IN](
      address: InetSocketAddress,
      factory: TypedChannelFactory[OUT, IN],
      name: String = ""
  ): NetConnection[UDP[OUT], UDP[IN]] = {
    val channel: TypedUDPChannel[UDP[OUT], UDP[IN]] =
      UDPChannel.connect[OUT, IN](address, factory)
    channels.NetConnection(channel, name)
  }

}
