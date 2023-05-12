package ox.eieio

import io.threadcso._
import io.threadcso.channel.Closed
import ox.eieio.options._
import ox.eieio.types._

import java.net._
import java.nio.ByteBuffer
import java.nio.channels._
import scala.annotation.tailrec
import io.SourceLocation._
  
object NetDatagramChannel extends ox.logging.Log("DatagramChannel")
{  
   def bound(address: InetSocketAddress): NetDatagramChannel =
   { val channel = new NetDatagramChannel(DatagramChannel.open())
     channel.bind(address) 
     channel
   }
   
   def connected(address: InetSocketAddress): NetDatagramChannel =
   { val channel = new NetDatagramChannel(DatagramChannel.open())
     channel.connect(address) 
     channel
   }
   
   def multicast(address: InetSocketAddress): NetMulticastChannel =
       multicast(address, address.getAddress)
       
   def multicast(address: InetSocketAddress, interface: InetAddress): NetMulticastChannel =
   { val channel = new NetMulticastChannel(DatagramChannel.open(options.IPv4))
     val ni      = NetworkInterface.getByInetAddress(interface)
     channel.setOption(SO_REUSEADDR, value = true)
     channel.bind(address) 
     channel.setNI(ni)
     channel
   }   
   
}
  
class NetMulticastChannel(_channel:  DatagramChannel) extends 
      NetDatagramChannel(_channel) with MulticastConnector
{
    /** Join the multicast group at `address` */
    def join(group: InetAddress): MembershipKey =
    { channel.join(group, ni)
    }
 
    def join(group: InetAddress, source: InetAddress): MembershipKey =
    { channel.join(group, ni, source)
    }
    
    /** The network interface */
    private
    var ni: NetworkInterface = null.asInstanceOf[NetworkInterface]
        
    def setNI(interface: NetworkInterface): Unit =
    { ni = interface 
      channel.setOption[NetworkInterface](IP_MULTICAST_IF, ni)
    }
    
    def getNI: NetworkInterface = ni

    override 
    def toString: String = "NetMulticastChannel(%s) (%s, NI: %s)".format(channel.toString, options, ni)
}

/** Main proxy class for datagram channels, which are synchronous
    in this implementation.Syn
*/
class NetDatagramChannel(val channel:  DatagramChannel) extends DatagramConnector
{ import ox.eieio.options._

 /** The most recent exception */
 var lastException: Throwable = null
 
 /** The most recent exception */
 def getLastException: Throwable = lastException
  
 /** The options set on the underlying channel */
 def options: String = "rcvbuf: %d, sndbuf: %d, Reusing: %b, IP_M_IF: %s".format(
           channel.getOption(SO_RCVBUF), 
           channel.getOption(SO_SNDBUF),
           channel.getOption(SO_REUSEADDR),
           channel.getOption(IP_MULTICAST_IF)
           )
 
 override 
 def toString: String = "NetDatagramChannel(%s) (%s)".format(channel.toString, options)
 
 /** Low-level send of datagram `buffer` to `address`.
     Throws `ox.cso.channels.Closed` if the underlying socket has been closed,
     or the `address` is not reachable or the destination address
     rejected the datagram. The variable `lastException: Throwable` 
     can be used to find out exactly what went wrong.
 */
 def send(buffer: BUFFER, address: SocketAddress): Int =
 {   lastException = null
     if (channel.isOpen) 
     {  
        try 
        { val n = channel.send(buffer, address)
          if (n<0) throw new Closed(toString)
          n
        }
        catch 
        { case exn: java.lang.Exception => 
                    lastException = exn
                    NetDatagramChannel.finest(exn)
                    throw new Closed(toString)
        }
     }
     else
        throw new Closed(toString)
 }

  /** Low-level receive of datagram `buffer` from the socket to which
    * this datagram channel is connected.
    *
    * Throws `Closed` if the underlying channel has been closed.
    */
  def receive(buffer: BUFFER): SocketAddress = {
    buffer.clear
    try {
      val addr = channel.receive(buffer)
      buffer.flip
      addr
    } catch {
      case exn: java.lang.Exception =>
        lastException = exn
        throw new Closed(toString)
    }
  }

        
 // Delegated

  /** Connect to the address */
  def connect(address: SocketAddress): Unit = channel.connect(address)

  /** Bind to the address -- prepare to listen at it for connections */
  def bind(address: SocketAddress): Unit = channel.bind(address)

  @inline def getDatagramAddress: SocketAddress = getRemoteAddress

  def getRemoteAddress: SocketAddress = channel.getRemoteAddress

  def getLocalAddress: SocketAddress = channel.getLocalAddress
 
 def setOption(opt: SocketOption[Integer], value: Int): Unit = channel.setOption(opt, value.asInstanceOf[Integer])
 def setOption(opt: SocketOption[java.lang.Boolean], value: Boolean): Unit = channel.setOption(opt, if (value) java.lang.Boolean.TRUE else java.lang.Boolean.FALSE )
 def setOption[T](opt: SocketOption[T], value: T): Unit = channel.setOption(opt, value)
 def getOption[T](opt: SocketOption[T]): T = channel.getOption(opt)
 
 /** Close the socket completely */
 def close(): Unit = channel.close()
 /** Close the input side of the socket  */
 def shutdownInput(): Unit  = channel.close()
 /** Close the output side of the socket  */
 def shutdownOutput(): Unit = channel.close()

        
 ///////////////// CSO interface ///////////////
 
 /**
     Forks a process that copies values from `in`,
     encodes them using the given `encoder`, and sends them as
     datagrams to the remote socket to which this channel is 
     currently connected. 
     
     The process terminates if the channel from which the `in` port
     is reading is closed; or if this (Datagram) channel has been closed
     or if there is another failure. The variable `lastException: Throwable`
     can be used to find out what happened. 
 */      
 def CopyToNet[T](in: ??[T], encoder: Encoder[T]): Unit =
 { val worker = 
   proc (this.toString+".CopyToNet")
   {  val buffers  = encoder.buffers
      val nbuffers = buffers.length
      val copy     = buffers.length!=1
      val buffer = if (!copy) 
                     buffers(0) 
                   else 
                   { var size  = 0 
                     for (buf<-buffers) size+=buf.capacity
                     ByteBuffer.allocate(size)
                   }
      def consolidate(): Unit =
      { if (copy) 
        { buffer.clear
          for (buf<-buffers) buffer.put(buf)
        }
      }
      
      NetDatagramChannel.fine("DatagramAddress: %s".format(getDatagramAddress))
      repeat {
        val value = in?()
        NetDatagramChannel.finest("in?() = %s".format(value))
        encoder.clear()
        encoder.encode(value)
        consolidate()
        var index  = 0
        val size   = try { send(buffer, getDatagramAddress) } catch { case exn: ClosedChannelException => -1 }
        if (size <= 0)
        { NetDatagramChannel.fine("CopyToNet: Peer output socket closed; shutting down input port")
          in.closeIn()
          stop
        }    
      }
      NetDatagramChannel.fine("CopyToNet: Peer input port closed; shutting down output socket")
      shutdownOutput()
   }
   
   toNet = fork(worker)
   ()
 }

/**
     Forks a process that copies `(value, addr)` pairs from `in`,
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
 def DatagramsToNet[T](in: ??[(T, SocketAddress)], encoder: Encoder[T]): Unit =
 { val worker = 
   proc (this.toString+".CopyToNet")
   {  val buffers  = encoder.buffers
      val nbuffers = buffers.length
      val copy     = buffers.length!=1
      val buffer = if (!copy) 
                     buffers(0) 
                   else 
                   { var size  = 0 
                     for (buf<-buffers) size+=buf.capacity
                     ByteBuffer.allocate(size)
                   }
      def consolidate() =
      { if (copy) 
        { buffer.clear
          for (buf<-buffers) buffer.put(buf)
        }
      }
      
      repeat {
        val (value, addr) = in?()
        NetDatagramChannel.finest(s"in?() = (${value} to ${addr})")
        encoder.clear()
        encoder.encode(value)
        consolidate()
        var index  = 0
        val size   = try { send(buffer, addr) } catch { case exn: ClosedChannelException => -1 }
        if (size<=0)
        { NetDatagramChannel.fine("CopyToNet: Peer output socket closed; shutting down input port")
          in.closeIn()
          stop
        }    
      }
      NetDatagramChannel.fine("CopyToNet: Peer input port closed; shutting down output socket")
      shutdownOutput()
   }
   
   toNet = fork(worker)
   ()
 }
 
 /**
     Forks a process that decodes (using `decoder`) and
     copies to `out` any datagrams arriving at the socket
     to which this `DatagramChannel` is bound.  The
     decoded values are paired with the socketaddresses
     at which they originated.
     
     The process terminates if the channel of which `out`
     is a port is closed; or if this (Datagram) channel
     has been closed; or if a datagram arrives that
     cannot be completely decoded by the `decoder` (this
     usually means that the datagram was silently
     truncated at the snding or the receiving end); or if
     there is another failure. The variable
     `lastException: Throwable` can be used to find out
     what happened.
 */
 def DatagramsFromNet[T](out: !![(T, SocketAddress)], decoder: Decoder[T]): Unit =
 { val copy = proc (this.toString+".DatagramsFromNet") {
       var decode: () => DecodeResult[T] = decoder.decode _
       val buffer=decoder.buffer
       var lastResult = null.asInstanceOf[T]
       NetDatagramChannel.fine("Datagrams from: %s".format(toString))

       def receive =
       { buffer.clear
         try {
           val addr = channel.receive(buffer)
           buffer.flip
           addr
         } catch {
           case exn: java.lang.Exception => 
           { lastException = exn
             NetDatagramChannel.finest(exn)
             null
           }
         }
       }
       
       @tailrec def processBuffer(sender: SocketAddress) : Unit =
       { if (sender!=null && buffer.remaining>0)
         { 
           var portOpen = true
           decode() match 
           { case ReadMore => 
                  // there's not enough to decode
                  { NetDatagramChannel.warning("CopyFromNet: Datagram Underflow(%s)".format(buffer))
                    return
                  }
             case ReadThen(cont) => 
                  // there's not enough to decode
                  { NetDatagramChannel.warning("CopyFromNet: Datagram Underflow(%s)".format(buffer))
                    return
                  }
             case Decoded(res: T @unchecked) => 
                  // decoding was successful
                  { portOpen = attempt { 
                        NetDatagramChannel.finest("CopyFromNet: Datagram Decoded (%s)".format(res))
                        lastResult = res
                        out!(res, sender)  
                        true
                    } { false }
                  }
             case Completed(stillOpen: Boolean) =>
                  // decoding was successful
                  // the datum has been disposed of by the decoder
                  // the decoder may have decided to close the port
                  {  
                     portOpen = stillOpen
                  }
           }             
           if (portOpen) 
           {  // continue issuing read requests
              NetDatagramChannel.finest("CopyFromNet: listening after (%s)\n%s".format(lastResult, buffer))
              return processBuffer(receive)
           }
           else
           {  // the output side of the peer's channel was closed by the peer
              // shut down any input processing at this end
              NetDatagramChannel.fine("CopyFromNet: Peer output port closed; shutting down input socket")
              buffer.clear()
              shutdownInput()
              out.closeOut()
              ()
           }
         }
         else
         { 
           NetDatagramChannel.fine("CopyFromNet: Peer socket closed; shutting down output port")
           buffer.clear()
           shutdownInput()
           out.closeOut()
           ()
         }            
       }
       // start the processing loop
       processBuffer(receive) 
     }
   
   fromNet = fork(copy)
   ()
 }
 
 def setAddressChannel(out: !![SocketAddress]): Unit =
 { addressChannel = out
 }
 
 private var addressChannel = null.asInstanceOf[!![SocketAddress]]
 
 /**
     As `DatagramsFromNet` except that the socket-of-origin of
     the datagram is written to the currently-set `addressChannel`
     immediately after the decoded datagram is written to `out`.
     It is, perhaps, somewhat smelly to set things up this way,
     but it's less smelly than having `CopyFromNet` throw an
     `UnsportedOperationException`, and less inconvenient (at
     least for testing) than not being able to treat datagram
     channels as `Connector`s. 
 */
 def CopyFromNet[T](out: !![T], decoder: Decoder[T]): Unit =
 { val copy = proc (this.toString+".CopyFromNet") {
       var decode: () => DecodeResult[T] = decoder.decode _
       val buffer=decoder.buffer
       var lastResult = null.asInstanceOf[T]
       def receive =
       { buffer.clear
         try {
           val addr = channel.receive(buffer)
           buffer.flip
           addr
         } catch {
           case exn: java.lang.Exception => 
           { lastException = exn
             NetDatagramChannel.finest(exn)
             null
           }
         }
       }
       
       @tailrec def processBuffer(sender: SocketAddress) : Unit =
       { if (sender!=null && buffer.remaining>0)
         { 
           var portOpen = true
           decode() match 
           { case ReadMore => 
                  // there's not enough to decode
                  { NetDatagramChannel.warning("CopyFromNet: Datagram Underflow(%s)".format(buffer))
                    return
                  }
             case ReadThen(cont) => 
                  // there's not enough to decode
                  { NetDatagramChannel.warning("CopyFromNet: Datagram Underflow(%s)".format(buffer))
                    return
                  }
             case Decoded(res: T @unchecked) => 
                  // decoding was successful
                  { portOpen = attempt { 
                        NetDatagramChannel.finest("CopyFromNet: Datagram Decoded (%s)".format(res))
                        lastResult = res
                        out!res  
                        if (addressChannel!=null) addressChannel!sender
                        true
                    } { false }
                  }
             case Completed(stillOpen: Boolean) =>
                  // decoding was successful
                  // the datum has been disposed of by the decoder
                  // the decoder may have decided to close the port
                  {  
                     portOpen = stillOpen
                  }
           }             
           if (portOpen) 
           {  // continue issuing read requests
              NetDatagramChannel.finest("CopyFromNet: listening after (%s)\n%s".format(lastResult, buffer))
              return processBuffer(receive)
           }
           else
           {  // the output side of the peer's channel was closed by the peer
              // shut down any input processing at this end
              NetDatagramChannel.fine("CopyFromNet: Peer output port closed; shutting down input socket")
              buffer.clear()
              shutdownInput()
              out.closeOut()
              ()
           }
         }
         else
         { 
           NetDatagramChannel.fine("CopyFromNet: Peer socket closed; shutting down output port")
           buffer.clear()
           shutdownInput()
           out.closeOut()
           ()
         }            
       }
       // start the processing loop
       processBuffer(receive) 
     }
   
   fromNet = fork(copy)
   ()
 }

 
}

















