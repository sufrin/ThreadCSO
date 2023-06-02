package ox.eieio

import java.net.SocketAddress
import java.nio.channels._
  
class   SyncNetChannelClient(address: SocketAddress) 
extends SyncNetChannel(SocketChannel.open())
{
 channel.connect(address)
}

