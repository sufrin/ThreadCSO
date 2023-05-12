package ox.eieio

import java.net.SocketAddress
import java.nio.channels._
  

/** A channel that can be used to send datagrams to a specific port */
class   SyncNetDatagramClient(address: SocketAddress) 
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
 
 channel.connect(address)
}

