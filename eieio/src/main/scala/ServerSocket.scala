package ox.eieio

import io.threadcso._

import java.net.InetSocketAddress
import java.nio.channels._
  
object ServerSocket extends Logger("ServerSocket")

/** Open an asynchronous network server socket on `port` with
    the given `backlog`.
    
    After starting (by invoking `accept`) this component handles
    each new connection by passing a `NetChannel` embodying the
    `AsynchronousSocketChannel` that is the result of the connection
    to `handler`).
    
    On closing (by invoking `close`) the underlying socket is closed,
    and `accept` will return.
*/
  
class ServerSocket(port: Int, backlog: Int, handle: NetChannel=>Unit)
{ /** The port on which this server is accepting connection requests. */
  val address = new InetSocketAddress(port)
  private val channel = AsynchronousServerSocketChannel.open(NetChannel.group).bind(address, backlog)
  override def toString = channel.toString
  
  private val term = new java.util.concurrent.Semaphore(1); term.acquire
  
  /** The most recent exception */
  var lastException: Throwable = null
  
  private var open = true
  
  private val completionHandler = new CompletionHandler[AsynchronousSocketChannel, Long] {
      def completed(socket: AsynchronousSocketChannel, attachment: Long): Unit =
      {   val chan = new NetChannel(socket)
          ServerSocket.fine("Accepted #%d %s".format(attachment, chan))
          handle(chan)                          // handle (quickly) 
          if (open) 
             channel.accept(attachment+1, this) // accept the next connection
          else
             channel.close
      }
      def failed(exn: Throwable, attachment: Long): Unit =
      {  lastException = exn
         term.release
         ServerSocket.severe("accept completion failed: %s".format(exn))
      }
  }
  
  /** Close this socket and allow `accept` to terminate. */
  def close:  Unit = { open=false; channel.close; term.release }
   
  /** Start accepting connection requests; returns only after `close`. */
  def accept: Unit = 
  { ServerSocket.fine("about to accept")
    channel.accept(0L, completionHandler)
    ServerSocket.fine("accepting (now waiting for server socket to close)")
    term.acquire 
  }
  
}





