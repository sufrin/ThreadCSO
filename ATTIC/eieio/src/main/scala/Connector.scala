package ox.eieio

import java.net.{InetAddress, NetworkInterface, SocketAddress, SocketOption}
import java.nio.channels._

import io.threadcso._
import ox.eieio.types._


/** A `Connector` (aka connection-provider) is a slight abstraction of one
        of three different kinds of channel provided by `EIEIO`.
  */
trait Connector {
  import io.threadcso.process.Process.Handle
  /** Start a process that copies values from `in`
        to the network, using the given `encoder`.
    */
  def CopyToNet[T](in: ??[T], encoder: Encoder[T]): Unit

  /** Start a process the copies to `out` values read from the network
        and decoded using the given decoder.
    */
  def CopyFromNet[T](out: !![T], decoder: Decoder[T]): Unit

  /** Handle on the running CopyToNet worker process (if it is synchronous) */
  var toNet: Handle = _

  /** Handle on the running CopyFromNet worker process (if it is synchronous) */
  var fromNet: Handle = _

  /** Set the given `opt` of the underlying socket to the given `value` */
  def setOption(opt: SocketOption[Integer], value: Int): Unit

  /** Set the given `opt` of the underlying socket to the given `value` */
  def setOption(opt: SocketOption[java.lang.Boolean], value: Boolean): Unit

  /** Set the given `opt` of the underlying socket to the given `value` */
  def setOption[T](opt: SocketOption[T], value: T): Unit

  /** Get the given `opt` of the underlying socket */
  def getOption[T](opt: SocketOption[T]): T

  /** Connect to the address */
  def connect(address: SocketAddress): Unit

  /** Bind to the address -- prepare to listen at it for connections */
  def bind(address: SocketAddress): Unit

  /** Last exception thrown by the underlying channel */
  def getLastException: Throwable

}

/** A `Connector` from which socket-labelled datagrams are copied.
 
    '''WARNING:''' It is unwise to mix the use of the `Copy`... and the `Datagrams`...
    methods on a channel expecting datagram traffic.
*/
trait DatagramConnector extends Connector {
  /** Read and decode datagrams from the net using the given decoder.
        Pair each decoded datagram with its socket of origin and 
        write it to `out`.
  */
  def DatagramsFromNet[T](out: !![(T, SocketAddress)], decoder: Decoder[T]): Unit
  
  /**
     A process that (when started) copies `(value, addr)` pairs from `in`,
     encodes each value using the given `encoder`, and sends it as
     a datagram to the remote socket whose `addr` is specified. 
     
     The process terminates if the channel from which the `in` port
     is reading is closed; or if this (Datagram) channel has been closed
     or if there is another failure. The variable `lastException: Throwable`
     can be used to find out what happened. 
     
     The following is an extract from a program that reflects datagrams sent it
     back to their source.

{{{     
    val channel = NetDatagramChannel.bound(address(host, port)) 
    channel.setOption(SO_RCVBUF, RCV)
    channel.setOption(SO_SNDBUF, SND)
    

    val reflect  = OneOne[(String, java.net.SocketAddress)]
    val fromHost = OneOne[(String, java.net.SocketAddress)]
    channel.DatagramsFromNet(fromHost, stringCodec.Decoder(bufferSize))
    channel.DatagramsToNet(reflect, stringCodec.Encoder(bufferSize))
    Console.println(channel)
    
    run (  proc ("fromHost") {
             repeat { 
               fromHost ? () match 
               { case (s, sender) => 
                      Console.println(s"\${sender} sent \${s}")
                      reflect!(s"bound reflected \${s}", sender)
               }
             } 
           }
        )
    reflect.close
    fromHost.close
}}}
     
 */      
   def DatagramsToNet[T](in: ??[(T, SocketAddress)], encoder: Encoder[T]): Unit

  /**
  Set the channel on which the socket addresses of datagrams passed to
  the program by `CopyFromNet` are sent in tandem. If this channel hasn't
  been set then the origins of datagrams read by `CopyFromNet` are lost.
  
  '''WARNING:'' It is unwise to mix the use of the `Copy`... and the `Datagrams`...
  methods on a channel expecting datagram traffic.
  */
  def setAddressChannel(out: !![SocketAddress]): Unit

}

/** A `DatagramConnector` from which multicast datagrams are copied */
trait MulticastConnector extends DatagramConnector {
  def join(group: InetAddress): MembershipKey

  def join(group: InetAddress, source: InetAddress): MembershipKey

  def setNI(interface: NetworkInterface): Unit
  def getNI: NetworkInterface
}
  









