package ox.eieio

import java.net.{SocketAddress, SocketOption}
import java.nio.channels._

import io.threadcso._
import io.threadcso.channel.Closed

import ox.eieio.types._

/**
  * Provides factory methods to construct synchronous network channels. */
object SyncNetChannel extends Logger("SyncNetChannel")
{
   /** Construct a synchronous network channel bound to the given socket address */
   def bound(address: SocketAddress): SyncNetChannel =
   { val channel = new SyncNetChannel(SocketChannel.open)
     channel.bind(address) 
     channel
   }

   /** Construct a synchronous network channel connected to the given socket address */
   def connected(address: SocketAddress): SyncNetChannel =
   { val channel = new SyncNetChannel(SocketChannel.open)
     channel.connect(address) 
     channel
   }
}


/**
  * A synchronous network channel associated with the given socket channel. */
class SyncNetChannel(_channel:  SocketChannel) extends Connector
{ import ox.eieio.options._

 /** The underlying `SocketChannel` */
 val channel                  = _channel
 
 /** The most recent exception */
 var lastException: Throwable = null
 
 /** The most recent exception */
 def getLastException = lastException

 
 /** The options set on the underlying channel */
 def options: String = "nodelay: %b, rcvbuf: %d, sndbuf: %d".format(
           channel.getOption(TCP_NODELAY), 
           channel.getOption(SO_RCVBUF), 
           channel.getOption(SO_SNDBUF))
 
 override 
 def toString = "SyncNetChannel(%s) (%s)".format(channel.toString, options)
 
 /** 
     Synchronous best-endeavour write of (possibly a prefix of) the `buffer`.
     {{{ PRE:  gettable buffer 
         POST: gettable buffer 
     }}} 
 */
 def write(buffer: BUFFER) = 
     if (channel.isOpen) 
     {
        try 
        { val n = channel.write(buffer)
          if (n<0) throw new Closed(toString) else n
        }
        catch 
        { case exn: java.util.concurrent.ExecutionException => 
                    { lastException = exn
                      throw new Closed(toString) }
        }
     }
     else
        throw new Closed(toString)
 
 /** Synchronous write of ''everything'' in the `buffer` 
     {{{ PRE:  gettable buffer 
         POST: gettable buffer && buffer.remaining == 0
     }}}
 */
 def writeAll(buffer: BUFFER) = 
     while (buffer.remaining>0) 
     {  SyncNetChannel.finest("write(%s)".format(buffer))
        val n = write(buffer)
        SyncNetChannel.finest("write(%s)=%d".format(buffer, n))
     }
         
 /** Synchronous best-endeavour read of up to `buffer.remaining` into the buffer:
     throws  `Closed` (if the channel is closed).
     
     {{{ PRE:     puttable buffer
         POST:    puttable buffer
     }}}
 */
 def read (buffer: BUFFER) = 
     if (channel.isOpen)
        { 
          SyncNetChannel.finest("Read(%s)".format(buffer))
          val n = channel.read (buffer)
          SyncNetChannel.finest("Read(%s)=%d".format(buffer, n))
          if (n<0) throw new Closed(toString) else n
        } 
     else
        throw new Closed(toString)  
        
        
 // Delegated
 
 /** Connect to the address*/
 def connect(address: SocketAddress) = channel.connect(address)
 
 /** Bind to the address -- prepare to listen at it for connections */
 def bind(address: SocketAddress) = channel.bind(address)

 
 def getRemoteAddress = channel.getRemoteAddress
 
 def setOption(opt: SocketOption[Integer], value: Int) = channel.setOption(opt, value.asInstanceOf[Integer])
 def setOption(opt: SocketOption[java.lang.Boolean], value: Boolean) = channel.setOption(opt, if (value) java.lang.Boolean.TRUE else java.lang.Boolean.FALSE )
 def setOption[T](opt: SocketOption[T], value: T) = channel.setOption(opt, value)
 def getOption[T](opt: SocketOption[T]): T = channel.getOption(opt)
 
 /** Close the socket completely */
 def close(): Unit = channel.close()
 /** Close the input side of the socket  */
 def shutdownInput(): Unit  = channel.shutdownInput
 /** Close the output side of the socket  */
 def shutdownOutput(): Unit = channel.shutdownOutput
 
        
 ///////////////// CSO interface ///////////////
 /** Fork a thread that repeatedly reads values from the given input port, encodes them, then writes them to the
   * channel. The input port is closed when the channel has been closed; and the channel is closed (and the process
   * terminates) if the input port's CSO channel has been closed. Writing to the network is synchronous. */
 def CopyToNet[T](in: ??[T], encoder: Encoder[T]): Unit = 
 { val worker = 
   proc (this.toString+".CopyToNet")
   {  val buffers  = encoder.buffers
      val nbuffers = buffers.length
      repeat {
        val value = in?()
        encoder.clear()
        encoder.encode(value)
        var index    = 0
        while (index!=nbuffers)
        { 
           val size = channel.write(buffers, index, nbuffers-index)
           if (size>0)
           { while (index < nbuffers && buffers(index).remaining==0) { index += 1 }
           } else
           { SyncNetChannel.fine("Peer output socket closed; shutting down input port")
             in.closeIn()
           }
        }     
      }
      SyncNetChannel.fine("Peer input port closed; shutting down output socket")
      channel.shutdownOutput()
   }
   toNet = fork (worker)
   SyncNetChannel.fine("CopyToNet forked: %s".format(toNet))
   ()
 }

  /** Fork a process that repeatedly reads and decodes values arriving on the channel, writing them
    * to the given output port. The output port is closed if the network channel closes, and the network
    * channel is closed if the output port's CSO channel is closed. Reading from the network is synchronous.
    *
    */
 def CopyFromNet[T](out: !![T], decoder: Decoder[T]): Unit =
 { val copy = proc (this.toString+".CopyFromNet") {
       var decode: () => DecodeResult[T] = decoder.decode _
       val buffer=decoder.buffer
       var lastResult = null.asInstanceOf[T]
       // DONE: removed the recursion
       def processBuffer(size: Int) : Unit =
       { SyncNetChannel.finer(s"processBuffer(${size}) [${buffer}]")
         var processing = size>0
         if (processing) buffer.flip
         while (processing)
         { 
           var portOpen = true
           while (portOpen && buffer.remaining>0) {
                 buffer.mark             // in case of a ReadMore
                 decode() match 
                 { case ReadMore => 
                        // there's not enough to decode
                        // compact what's present and read some more
                        { SyncNetChannel.finest("Reading more(%s)".format(buffer))
                          buffer.compact
                          SyncNetChannel.finest("Reading more compacted(%s)".format(buffer))
                          processing = (0<channel.read(buffer))
                          if (processing) buffer.flip
                        }
                   case ReadThen(cont) => 
                        // there was not enough to decode
                        // the decoder has already repositioned the buffer
                        // by compacting the partly-decoded prefix
                        { SyncNetChannel.finest("ReadThen (%s)".format(buffer))
                          // switch the decoder to the continuation for the next read
                          decode = cont
                          processing = (0<channel.read(buffer))
                          if (processing) buffer.flip
                        }
                   case Decoded(res: T @unchecked)  => 
                        // decoding was successful
                        // there may be (several) more decodeables in the buffer
                        // we don't compact at this stage
                        { portOpen = attempt { 
                              SyncNetChannel.finest("Decoded (%s)".format(res))
                              lastResult = res
                              out!res  
                              true
                          } { false }
                          // switch the decoder back to the original
                          decode = decoder.decode _
                        }
                   case Completed(stillOpen: Boolean) =>
                        // decoding was successful
                        // the datum has been disposed of by the decoder
                        // there may be (several) more decodeables in the buffer
                        // the decoder may have decided to close the port
                        {  
                           portOpen = stillOpen
                        }
                  }
             }
            // not portOpen || buffer.remaining==0               
            if (portOpen) 
            {  // continue issuing read requests
               SyncNetChannel.finest("Restarting after (%s)\n%s".format(lastResult, buffer))
               buffer.compact
               SyncNetChannel.finest("Compacted after (%s)\n%s".format(lastResult, buffer))
               processing = (0<channel.read(buffer))
               if (processing) buffer.flip
            }
            else
            {  // the output side of the peer's channel was closed by the peer
               // shut down any input processing at this end
               SyncNetChannel.fine("Peer output port closed; shutting down input socket")
               buffer.clear
               channel.shutdownInput
               out.closeOut()
               processing = false
            }
           }
           // 
           { 
             SyncNetChannel.fine("Peer socket closed; shutting down output port")
             buffer.clear
             channel.shutdownInput()
             out.closeOut()
             ()
           }            
       }
       // start the processing loop
       processBuffer(channel.read(buffer)) 
     }
   
   fromNet = fork(copy)
   SyncNetChannel.fine("CopyFromNet forked: %s".format(fromNet))
   ()
 }

 
}









