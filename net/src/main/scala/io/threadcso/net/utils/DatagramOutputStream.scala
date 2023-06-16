package io.threadcso.net.utils

import java.io.IOException
import java.net.SocketAddress
import java.nio.ByteBuffer
import java.nio.channels.DatagramChannel

object DatagramOutputStream {
  val log = new ox.logging.Log()

  def apply(channel: DatagramChannel, size: Int): DatagramOutputStream =
      new DatagramOutputStream(channel, size)
}

/**
  * A reusable buffered datagram output stream that flushes a datagram
  * to its given `channel`.
  */
class DatagramOutputStream(val channel: DatagramChannel, size: Int)  extends ByteBufferOutputStream(size) {
  val log = DatagramOutputStream.log
  // @inline private logging = log.logging
  val logging = false

  var address: SocketAddress = null

  /** Prepare to buffer a new datagram that will, when flushed,
    * be sent to the given address. If the `channel` is already
    * connected to that address then no security check is performed
    * by the security manager.
    */
  def newDatagram(address: SocketAddress): Unit = {
    if (log.logging) log.finest(s"DatagramOutputStream.newDatagram($address)")
    if (address eq null) {
      if (channel.isConnected)
          this.address = channel.getRemoteAddress
      else
          throw new IllegalArgumentException(s"newDatagram(null) for unconnected $channel")
    } else {
      log.finest(s"Assigning $address for this datagram")
      this.address = address
    }
    buffer.clear()
  }

  /** Flush the buffer to the `channel` as a datagram addressed to
    * the specified address.
    */
  override def flush(): Unit = {
    if (logging) log.finest(s"flushing")
    super.flush()
    send(buffer, this.address)
  }

  def send(buffer: ByteBuffer, address: SocketAddress): Unit = {
    var count = 0
    if (channel.isOpen) {
      buffer.flip()
      if (log.logging) log.finest(s"UDP.send($buffer) to $address")
      while (buffer.hasRemaining())
        count += channel.send(buffer, address)
      if (log.logging) log.finest(s"DatagramOutputStream($count) to $address")
      count
    } else {
      throw new IOException(s"DatagramOutputStream: $channel is not open")
    }

  }
}
