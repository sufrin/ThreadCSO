import io.threadcso._
import io.threadcso.channel.Closed
import ox.eieio.Factories._
import ox.eieio.types._
import ox.logging.Log

import java.net.{SocketAddress, SocketOption}
import java.nio.channels._



/**
  * Provides factory methods to construct synchronous network channels.
  * (An experiment in factoring codecs.)
  *
  */
object BufferedSyncNetChannel extends Log("BufferedSyncNetChannel")
{
   /** Construct a synchronous network channel bound to the given socket address */
   def bound[OUT,IN](address: SocketAddress, encoderFactory: EncoderFactory[OUT], decoderFactory: DecoderFactory[IN]): BufferedSyncNetChannel[OUT,IN] =
   { val channel = new BufferedSyncNetChannel[OUT,IN](SocketChannel.open, encoderFactory, decoderFactory)
     channel.bind(address) 
     channel
   }

   /** Construct a synchronous network channel connected to the given socket address */
   def connected[OUT,IN](address: SocketAddress, encoderFactory: EncoderFactory[OUT], decoderFactory: DecoderFactory[IN]): BufferedSyncNetChannel[OUT,IN] =
   { val channel = new BufferedSyncNetChannel[OUT,IN](SocketChannel.open, encoderFactory, decoderFactory)
     channel.connect(address) 
     channel
   }


}



/**
  * A synchronous network channel associated with the given socket channel. */
class BufferedSyncNetChannel[OUT,IN](_channel:  SocketChannel, encoderFactory: EncoderFactory[OUT], decoderFactory: DecoderFactory[IN]) {

  import ox.eieio.options._

  /** The underlying `SocketChannel` */
  val channel = _channel

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
  def toString = "BufferedSyncNetChannel(%s) (%s)".format(channel.toString, options)

  /**
    * Synchronous best-endeavour write of (possibly a prefix of) the `buffer`.
    * {{{
    *     PRE:  gettable buffer
    *     POST: gettable buffer
    * }}}
    */
  def write(buffer: BUFFER) =
     if (channel.isOpen) 
     {  //BufferedSyncNetChannel.finest(s"write($buffer)")
        try 
        { val n = channel.write(buffer)
          //BufferedSyncNetChannel.finest(s"write($buffer)=$n")
          if ( n <0) throw new Closed(toString) else n
        }
        catch 
        { case exn: java.util.concurrent.ExecutionException => 
                    { lastException = exn
                      throw new Closed(toString) }
        }
     }
     else
        throw new Closed(toString)
 
 /** Synchronous write of ''everything'' in the `buffer`* {{{ PRE:  gettable buffer
   *
    POST: gettable buffer && buffer.remaining == 0
   *RESULT: old(buffer).remaining
   *
  }}}
 */
  def writeAll(buffer: BUFFER): Int = {
     var count = 0
     while (buffer.remaining>0)
     {  BufferedSyncNetChannel.finest(s"writeAll(%s)[${buffer.remaining}]".format(buffer))
       val n = write(buffer)
       count += n
       BufferedSyncNetChannel.finest("writeAll(%s)=%d".format(buffer, n))
     }
     count
 }

  /** Synchronous best-endeavour read of up to `buffer.remaining` into the buffer:
    * throws  `Closed` (if the channel is closed).
    *
    * {{{
    * PRE:     puttable buffer
    * POST:    puttable buffer
    * }}}
 */
  def read (buffer: BUFFER) =
     if (channel.isOpen)
        { 
          BufferedSyncNetChannel.finest("Read(%s)".format(buffer))
          val n = channel.read (buffer)
          BufferedSyncNetChannel.finest("Read(%s)=%d".format(buffer, n))
          if (n <0) throw new Closed(toString) else n
        } 
     else
        throw new Closed(toString)  
        
        
  /** Built at connect/bind time  */
  var encoding: Encoding[OUT] = _
  /** Built at connect/bind time  */
  var decoding: Decoding[IN]  = _

  import io.threadcso.process._

  var toNet, fromNet: Process.Handle = _
 
 /** Connect to the addres s*/
  def connect(address: SocketAddress) = {
   encoderFactory.setOptions(channel)
   decoderFactory.setOptions(channel)
   channel.connect(address)
   encoding = encoderFactory.makeEncoder(channel)
   decoding = decoderFactory.makeDecoder(channel)
 }
 
 /** Bind to the address -- prepare to listen at it for connections */
  def bind(address: SocketAddress) = {
   encoding = encoderFactory.makeEncoder(channel)
   decoding = decoderFactory.makeDecoder(channel)
   encoderFactory.setOptions(channel)
   decoderFactory.setOptions(channel)
   channel.bind(address)
 }
 
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
 /** A process that repeatedly reads values from the given input port, encodes them, then writes them to the
   * channel. The input port is closed when the channel has been closed; and the channel is closed (and the process
   * terminates) if the input port's CSO channel has been closed. Writing to the network is synchronous. */
  def CopyToNet(in: ??[OUT]): PROC =  proc (this.toString +".CopyToNet") {
      BufferedSyncNetChannel.fine(s"CopyToNet($in)")
      repeat {
        val value = in ? ()
        encoding.clear()
        val buffers = encoding.encode(value)
        var sending = true
        // the encodinng may do the sending itself, and yield Nil
        for { buffer <- buffers if buffer.hasRemaining }
        {  val size = writeAll(buffer)
           if (size <0)
           { BufferedSyncNetChannel.finest(s"CopyToNet: wrote $size, so shutting down input port")
             in.closeIn()
           } else {
             sending = size >0
             BufferedSyncNetChannel.finest(s"CopyToNet: wrote $size")
           }
        }     
      }
      // !in.canInput
      BufferedSyncNetChannel.fine("Peer input port has closed; shutting down output socket")
      channel.shutdownOutput()
   }

  /** A process that repeatedly reads and decodes values arriving on the channel, writing them
    * to the given output port. The output port is closed if the network channel closes, and the network
    * channel is closed if the output port's CSO channel is closed. Reading from the network is synchronous.
    *
    */
  def CopyFromNet(out: !![IN]): PROC = proc (this.toString +".CopyFromNet") {
   BufferedSyncNetChannel.fine(s"CopyToNet($out)")
   var going = decoding.canDecode
   repeat (going) {
     BufferedSyncNetChannel.fine(s"CopyFromNet.decode()")
     val decoded = decoding.decode()
     going = decoding.canDecode
     BufferedSyncNetChannel.fine(s"CopyFromNet.decoded($going): $decoded")
     if (going) {
         out ! decoded
         BufferedSyncNetChannel.fine(s"CopyFromNet.out ! $decoded")
     }
   }
   BufferedSyncNetChannel.fine(s"CopyFromNet terminating")
   out.closeOut()
 }

}









