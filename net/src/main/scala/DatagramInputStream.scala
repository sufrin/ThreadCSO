package ox.net

import ox.net.UDPChannel.{finest, logging}

import java.net.SocketAddress
import java.nio.ByteBuffer
import java.nio.channels.DatagramChannel

object DatagramInputStream extends ox.logging.Log("DatagramOutputStream") {
  def apply(channel: DatagramChannel, size: Int): DatagramInputStream =
    new DatagramInputStream(channel, size)
}

class DatagramInputStream(val channel: DatagramChannel, size: Int) extends ByteBufferInputStream(size) {

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
  def receive(buffer: ByteBuffer): SocketAddress = {
      buffer.clear
      val sourceAddress = channel.receive(buffer)
      if (logging) finest(s"received $buffer from $sourceAddress")
      buffer.flip
      sourceAddress
  }
}
