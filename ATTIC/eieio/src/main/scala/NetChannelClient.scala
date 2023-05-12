package ox.eieio

import java.net.SocketAddress
import java.nio.channels._

/** Construct a `NetChannel` connected to a socket with the given address. */
class   NetChannelClient(address: SocketAddress) 
extends NetChannel(AsynchronousSocketChannel.open(NetChannel.group))
{ 
  channel.connect(address).get
}

