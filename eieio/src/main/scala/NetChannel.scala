package ox.eieio

import java.net.{SocketAddress, SocketOption}
import java.nio.channels._
import java.util.concurrent.Executors.defaultThreadFactory

import io.threadcso._
import io.threadcso.basis.getPropElse
import io.threadcso.channel.Closed
import ox.eieio.types._

object NetChannelRead  extends Logger("NetChannelRead") 
object NetChannelWrite extends Logger("NetChannelWrite")

/**
  * Provides two factory methods for constructing `NetChannel`s. A `NetChannel` is the
  * CSO interface to (and wrapper for) a `java.nio.channels.AsynchronousSocketChannel`
  */
object NetChannel extends Logger
{  /**
     The `AsynchronousChannelGroup` used in the construction of
     asynchronous channels. This is the pool of threads (re)used
     to manage outstanding input/output requests. Threads in the
     pool are built using
     `java.util.concurrent.Executors.defaultThreadFactory`.  The
     pool size can be set by defining the JVM property
     `eieio.threadpool.size` before the first `NetChannnel`,
     `NetChannnelClient`, or `ServerSocket` is constructed. The
     default pool size is 2.
   */
   lazy val group = 
        AsynchronousChannelGroup.
              withFixedThreadPool(getPropElse("eieio.threadpool.size", _.toInt)(2), defaultThreadFactory)

  /** Construct a `NetChannel` bound to the given `SocketAddress` */
   def bound(address: SocketAddress): NetChannel =
   { val channel = new NetChannel(AsynchronousSocketChannel.open(group))
     channel.bind(address) 
     channel
   }

  /** Construct a `NetChannel` connected to the given `SocketAddress` */
  def connected(address: SocketAddress): NetChannel =
   { val channel = new NetChannel(AsynchronousSocketChannel.open(group))
     channel.connect(address) 
     channel
   }
}


/** A `NetChannel` is the CSO interface to (and wrapper for) a `java.nio.channels.AsynchronousSocketChannel`
  * */
class NetChannel(_channel: AsynchronousSocketChannel) extends Connector
{ import ox.eieio.options._

  /** The underlying `AsynchronousSocketChannel` */
  val channel                  = _channel
  
  /** The most recent exception */
  var lastException: Throwable = null

  /** The most recent exception */
  def getLastException = lastException
     
  /** The options set on the underlying channel */
  def options: String = "rcvbuf: %d, sndbuf: %d".format(channel.getOption(SO_RCVBUF), channel.getOption(SO_SNDBUF))
  
  override 
  def toString = "NetChannel(%s) (%s)".format(channel.toString, options)

  /** Construct a write-completion handler for the asynchronous socket channel that reads a `T` from the given
    * input port, and uses the given `Encoder` to prepare it for transmission on the channel.  */
  private def copyToNetHandler[T](in: ??[T], encoder: Encoder[T]):
          CompletionHandler[java.lang.Long, Any] = 
          new CompletionHandler[java.lang.Long, Any] { handler =>
             val buffers  = encoder.buffers
             val nbuffers = buffers.length
             def completed(size: java.lang.Long, a: Any): Unit =
             {   NetChannelWrite.finest("(%s) write completed(%d) (%s)".
                    format(Thread.currentThread.getName, size, buffersToString(buffers)))
                 if (size>0) {
                    // just written size bytes to the channel: see if any remain buffered
                    var index = 0
                    while (index < nbuffers && buffers(index).remaining==0) { index += 1 }
                    // index==nbuffers || buffers(index).remaining!=0
                    NetChannelWrite.finest( s"$index $nbuffers ${if (index!=nbuffers) s"${buffers(index).remaining}" else "NO BUFFERS"}")
                    if (index!=nbuffers)
                       // there were buffered bytes left to write to the channel
                       channel.write(buffers, index, nbuffers-index, 0, null, channel, handler)
                    else
                    {  // the most recent value was encoded and written
                       // read another value from `in`, encode it into the buffers, and start the async write
                       val portOpen =
                         attempt  { val v = in?()
                                    copy(v, encoder, handler)
                                    true
                                  } 
                                  { NetChannelWrite.fine("copyToNet in port closed; about to channel.shutdownOutput")
                                    shutdownOutput()
                                    false 
                                  }
                    }                              
                 }
                 else {
                   NetChannelWrite.finest("copyToNet wrote %d bytes: about to in.closeIn".format(size))
                   in.closeIn()
                 }
             }
             def failed(exn: Throwable, a: Any): Unit =
             {  lastException = exn
                NetChannelWrite.info("copyToNet failed: %s".format(exn))
                in.closeIn()
             }
          }
  
      /** Encode and ''start'' to  write the `value` asynchronously to the channel using the `buffers`
          provided by the  `encoder`. The completion handler deals with any buffered bytes that are not
          shipped by the first `write`.
      */
     
      private def copy[T](value: T, encoder: Encoder[T], handler: CompletionHandler[java.lang.Long, Any]): Unit =
      {   val bufs = encoder.buffers
          encoder.clear()
          encoder.encode(value)
          channel.write(bufs, 0, bufs.length, 0, null, null, handler)
      }

      /**
          Start the copying of values from `in` to the underlying
          asynchronous network channel until `in` closes.

          For best performance it is advisable (though not mandatory) to implement `in` as a 
          buffered channel, so that the generation of values to be output can be decoupled
          from their transmission to the network. 
                  
          The CSO process forked by this method reads only once from `in`
          and then terminates; thereafter reading from `in` is performed on one of the threads
          from the pooled group devoted to asynchronous I/O by `java.nio.Asynchronous...`. If the process 
          producing the values read from `in` is too slow, and the pooled group is insufficiently
          large then other asynchronous I/O processing might be disrupted.

          @see `NetChannel.group`.
                    
      */
      def CopyToNet[T](in: ??[T], encoder: Encoder[T]): Unit =
      {  val handler = copyToNetHandler(in, encoder)
         toNet = fork ( proc ("NetChannel.CopyToNet first in?") { copy(in?(), encoder, handler) } )
         NetChannelWrite.fine("CopyToNet forked %s".format(toNet))
      }

  ////////////////////////////////////
      
  /** Construct a read-completion handler for asynchronous reading and decoding from the channel. When the given
    * `Decoder` signals that it has decoded a complete byte representation of a `T` from the channel, the decoded value
    * is written to the given output port, and (unless the channel has been closed) the next asynchronous read
    * on the channel is started. */
  private
  def readHandler[T](out: !![T], decoder: Decoder[T]):
      CompletionHandler[Integer, AsynchronousSocketChannel] = 
      new CompletionHandler[Integer, AsynchronousSocketChannel] {
         // the decoder is changed when a ReadThen(continuation) is encountered
         var decode: () => DecodeResult[T] = decoder.decode _
         val buffer = decoder.buffer
         var n = 0
         def completed(size: Integer, channel: AsynchronousSocketChannel): Unit =
         {   NetChannelRead.finest("(%s) completed(#%d, size=%d) (%s)".
                format(Thread.currentThread.getName, n, size, buffer))
             n+=1   
             if (size>0) {
                // there is material in the buffer; decode it
                var portOpen       = true                 // state of the out port
                var lastResult: T  = null.asInstanceOf[T] // last thing we decoded
                // prepare to decode from the buffer
                buffer.flip
                // 
                while (portOpen && buffer.remaining>0) {
                      buffer.mark             // in case of a ReadMore
                      NetChannelRead.finest("%d Marked (%s)".format(n, buffer))
                      decode() match 
                      { case ReadMore => 
                             // there's not enough to decode
                             // compact what's present and read some more
                             { NetChannelRead.finest("%d Reading more(%s)".format(n, buffer))
                               buffer.compact
                               NetChannelRead.finest("%d Reading more compacted(%s)".format(n, buffer))
                               channel.read(buffer, channel, this)
                               return
                             }
                        case ReadThen(cont) => 
                             // there was not enough to decode
                             // the decoder has already repositioned the buffer
                             // by compacting the partly-decoded prefix
                             { NetChannelRead.finest("%d ReadThen (%s)".format(n, buffer))
                               // switch the decoder to the continuation for the next read
                               decode = cont
                               channel.read(buffer, channel, this)
                               return
                             }
                        case Decoded(res: T @unchecked) => 
                             // decoding was successful
                             // there may be (several) more decodeables in the buffer
                             // we don't compact at this stage
                             { // switch the decoder back to the original
                               decode = decoder.decode _
                               portOpen = attempt { 
                                   lastResult = res
                                   NetChannelRead.finest("%d Decoded (%s)".format(n, res))
                                   out!res  
                                   true
                               } { false }
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
                    NetChannelRead.finest("%d Restarting after (%s)\n%s".format(n, lastResult, buffer))
                    buffer.compact
                    NetChannelRead.finest("%d Compacted after (%s)\n%s".format(n, lastResult, buffer))
                    channel.read(buffer, channel, this)
                 }
                 else
                 {  // the output side of the peer's channel was closed by the peer
                    // shut down any input processing at this end
                    shutdownInput()
                    buffer.clear
                    out.closeOut()
                 }
             }
             else 
             { // the output side of the peer's channel was closed by the peer
               NetChannelRead.fine("Channel closed: size=%d buffer=%s".format(size, buffer))
               shutdownInput()
               buffer.clear()
               out.closeOut()
             }
         }
         def failed(exn: Throwable, attachment: AsynchronousSocketChannel): Unit =
         {  NetChannelRead.info("CopyFromNet failed: %s".format(exn))
            lastException = exn
            shutdownInput()
            buffer.clear()
            out.closeOut()
        }
      }
  
  /** 
  
      Start reading and decoding asynchronously, using `decoder`.
      Whenever a `value: T` is decoded on the underlying network
      input stream, it is passed to a consumer process by `out!value`.
      This method returns immediately.  Reading proceeds asynchronously.

      It is advisable (though not mandatory) to implement `out` as
      a buffered channel; for otherwise the asynchronous I/O pooled
      thread that runs the completion code that writes vaues to
      `out` has to wait until each is consumed before it can be
      reused.  If the process consuming the values  written to
      `out` is too slow and the pooled group is insufficiently large
      then other asynchronous I/O processing might be disrupted.
      
      @see `NetChannel.group`.

  */   
  def CopyFromNet[T](out: !![T], decoder: Decoder[T]): Unit =
  { 
    channel.read(decoder.buffer, channel, readHandler(out, decoder))
    ()
  }
  
  // Delegated
  
  /** Connect to the address*/
  def connect(address: SocketAddress): Unit= channel.connect(address)
  
  /** Bind to the address -- prepare to listen at it for connections */
  def bind(address: SocketAddress): Unit = channel.bind(address)

  def getRemoteAddress: SocketAddress = channel.getRemoteAddress
  
  def setOption(opt: SocketOption[Integer], value: Int): Unit = channel.setOption(opt, value.asInstanceOf[Integer])
  def setOption(opt: SocketOption[java.lang.Boolean], value: Boolean): Unit  = channel.setOption(opt, if (value) java.lang.Boolean.TRUE else java.lang.Boolean.FALSE )
  def setOption[T](opt: SocketOption[T], value: T): Unit = channel.setOption(opt, value)
  def getOption[T](opt: SocketOption[T]): T = channel.getOption(opt)
  
  /** Close the socket completely */
  def close(): Unit = channel.close()
  /** Close the input side of the socket  */
  def shutdownInput(): Unit  = if (channel.isOpen) channel.shutdownInput()
  /** Close the output side of the socket  */
  def shutdownOutput(): Unit = if (channel.isOpen) channel.shutdownOutput
  
  
  ////////////////////////////////////////////////////////////////////////////////
  // Low-level synchronous methods -- mostly for testing
  ////////////////////////////////////////////////////////////////////////////////
  
  /** 
      Synchronous best-endeavour write of (possibly a prefix of) the `buffer`.
      {{{
          PRE:     gettable buffer
          POST:    gettable buffer
          RETURNS: number of bytes, n,  written (if >=0)
          THROWS:  Closed, if n<0, or a concurrent execution exception is thrown, or the channel is closed
      }}} 
  */
  def write(buffer: BUFFER): Int =
      if (channel.isOpen) 
      {
         try 
         { val n = channel.write(buffer).get
           if (n<0) throw new Closed(toString)
           n
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
    * {{{
    *     PRE:  gettable buffer
    *     POST: gettable buffer && buffer.remaining == 0
    *     RETURNS: number of bytes, n,  written (if >=0)
    *     THROWS:  Closed, if n<0, or a concurrent execution exception is thrown, or the channel is closed      }}}
  */
  def writeAll(buffer: BUFFER): Int = {
      var written = 0
      NetChannelWrite.finest(s"writeAll($buffer)")
      while (buffer.remaining>0) 
      {
         written += write(buffer)
      }
      NetChannelWrite.finest(s"writeAll=$written")
      written
  }

  /** Synchronous best-endeavour read of up to `buffer.remaining` into the buffer:
      throws  `Closed` (if the channel is closed).
      
      (if the channel is closed)
      
      {{{ PRE:     puttable buffe
          POST:    puttable buffer
      }}}
  */
  def read (buffer: BUFFER) = 
      if (channel.isOpen)
         { 
           NetChannelRead.finest("Read(%s)".format(buffer))
           val n = channel.read (buffer).get
           NetChannelRead.finest("Read(%s)=%d".format(buffer, n))
           if (n<0) throw new Closed(toString) else n
         } 
      else
         throw new Closed(toString)

  
}

  









