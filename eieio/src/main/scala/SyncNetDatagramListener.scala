package ox.eieio

import java.net.SocketAddress
import java.nio.channels._
  
/** A datagram channel bound to a particular port */
class   SyncNetDatagramListener(address: SocketAddress) 
extends NetDatagramChannel(DatagramChannel.open())
{ import ox.eieio.options._
 override def options: String = 
    "rcvbuf: %d, sndbuf: %d, bdcast: %b, tos: %d, loopback: %b".format(
           channel.getOption(SO_RCVBUF), 
           channel.getOption(SO_SNDBUF),
           channel.getOption(SO_BROADCAST),
           channel.getOption(IP_TOS),
           channel.getOption(IP_MULTICAST_LOOP)
           )
 channel.bind(address)
}
 
