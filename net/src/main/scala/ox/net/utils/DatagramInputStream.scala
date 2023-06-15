package ox.net.utils

import java.net.SocketAddress
import java.nio.ByteBuffer
import java.nio.channels.DatagramChannel

object DatagramInputStream {
  val log     = new ox.logging.Log()

  def apply(channel: DatagramChannel, size: Int): DatagramInputStream =
      new DatagramInputStream(channel, size)
}

/**
  *   An input stream view of a `DatagramChannel` with a buffer of the given `size`.
  */
class DatagramInputStream(val channel: DatagramChannel, size: Int) extends ByteBufferInputStream(size) {
  import DatagramInputStream._
  /**
    * Receive the next datagram on the associated `channel`, and return the address
    * from which it was sent.
    */
  def receive(): SocketAddress = {
    receive(byteBuffer)
  }

  /**
    * Low-level receive of datagram `buffer` from the socket to which
    * this datagram channel is connected.
    *
    * @return the remote address from which the datagram was sent
    *
    *         TODO: Heuristic to check absence of overrun?
    *         Heuristic: if there is no space left after the receive then unless
    *         the send was EXACTLY the right size there must have been a silent
    *         discard of some extra bytes. This heuristic is only useful
    *         for time-saving, not for correctness, because the codecs themselves
    *         doing the decoding should check well-formedness.
    *         A more useful heuristic might be to enlarge the buffer spontaneously
    *         after the reception of a "nearly overflowing" packet.
    */
  @inline private def receive(buffer: ByteBuffer): SocketAddress = {
      buffer.clear
      val sourceAddress = channel.receive(buffer)
      if (log.logging) log.finest(s"received $buffer from $sourceAddress")
      buffer.flip
      sourceAddress
  }
}
