package io.threadcso.net

import io.threadcso.net.UDPTransport.UDP
import io.threadcso.net.transport.{
  NetConnection,
  TypedTransportFactory,
  TypedUDPTransport
}

import java.net.InetSocketAddress

object UDPConnection {

  /**
    * A network connection bound to `address` that sends/receives `UDP[OUT]/UDP[IN]`
    * @see NetConnection
    */
  def bind[OUT, IN](
                     address: InetSocketAddress,
                     factory: TypedTransportFactory[OUT, IN],
                     name: String = ""
  ): NetConnection[UDP[OUT], UDP[IN]] = {
    val channel: TypedUDPTransport[UDP[OUT], UDP[IN]] =
      UDPTransport.bind[OUT, IN](address, factory)
    transport.NetConnection(channel, name)
  }

  /**
    * A network connection connected to `address` that sends/receives `UDP[OUT]/UDP[IN]`
    * @see NetConnection
    */
  def connect[OUT, IN](
                        address: InetSocketAddress,
                        factory: TypedTransportFactory[OUT, IN],
                        name: String = ""
  ): NetConnection[UDP[OUT], UDP[IN]] = {
    val channel: TypedUDPTransport[UDP[OUT], UDP[IN]] =
      UDPTransport.connect[OUT, IN](address, factory)
    transport.NetConnection(channel, name)
  }

}
