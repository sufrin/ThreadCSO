package ox.net

import ox.net.ByteBufferOutputStream.{finest, logging}

import java.io.IOException
import java.net.SocketAddress
import java.nio.ByteBuffer
import java.nio.channels.DatagramChannel

object DatagramOutputStream extends ox.logging.Log("DatagramOutputStream") {
  def apply(channel: DatagramChannel, size: Int): DatagramOutputStream =
      new DatagramOutputStream(channel, size)
}

/**
  * A reusable buffered datagram output stream that flushes a datagram
  * to its given `channel`.
  */
class DatagramOutputStream(val channel: DatagramChannel, size: Int)
    extends ByteBufferOutputStream(size) {

  var address: SocketAddress = null

  /** Prepare to buffer a new datagram that will, when flushed,
    * be sent to the given address. If the `channel` is already
    * connected to that address then no security check is performed
    * by the security manager.
    */
  def newDatagram(address: SocketAddress): Unit = {
    if (address eq null) {
      if (channel.isConnected)
          this.address = channel.getRemoteAddress
      else
          throw new IllegalArgumentException(s"newDatagram(null) for unconnected $channel")
    } else {
      this.address = address
    }
    if (logging) finest(s"DatagramOutputStream.newDatagram($address)")
    buffer.clear()
  }

  /** Flush the buffer to the `channel` as a datagram addressed to
    * the specified address.
    */
  override def flush(): Unit = {
    super.flush()
    send(buffer, address)
  }

  def send(buffer: ByteBuffer, address: SocketAddress): Unit = {
    var count = 0
    if (channel.isOpen) {
      buffer.flip()
      if (logging) finest(s"UDP.send($buffer)")
      while (buffer.hasRemaining())
        count += channel.send(buffer, address)
      finest(s"DatagramOutputStream($count) to $address")
      count
    } else {
      throw new IOException(s"DatagramOutputStream: $channel is not open")
    }

  }
}
