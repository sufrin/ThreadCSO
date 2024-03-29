package io.threadcso.net.utils

import java.io.InputStream
import java.nio.ByteBuffer

object ByteBufferInputStream {
    val log = new ox.logging.Log()
    def apply(size: Int): ByteBufferInputStream = new ByteBufferInputStream(size)
}

/**
  * `InputStream` view of a single, allocated, `ByteBuffer`. Intended for
  * use only in the decoding of a single datagram read into a single
  * ByteBuffer.
  *
  * @param size the size of the ByteBuffer allocated for this stream
  */
class ByteBufferInputStream(size: Int) extends InputStream {
    private val log = ByteBufferInputStream.log
    //@inline private def logging = log.logging
    val logging = false

    override def toString: String = s"ByteBufferInputStream($size) (remaining: ${byteBuffer.remaining()})"
    val buf                    = new Array[Byte](size)
    val byteBuffer: ByteBuffer = ByteBuffer.wrap(buf)

    override def read(): Int = {
      if (logging) log.finest(s"BBIS.read(): $byteBuffer ${byteBuffer.remaining()}")
      if (byteBuffer.hasRemaining) {
        val b =  byteBuffer.get()
        if (logging) log.finest(f"BBIS.read() = \\0x$b%04x")
        b
      } else -1
    }

  override def read(buf: Array[Byte]): Int = {
    if (logging) log.finest(s"BBIS.read(buf): $byteBuffer ${byteBuffer.remaining()}")
    val rem = byteBuffer.remaining
    if (rem==0) -1 else {
      byteBuffer.get(buf, 0, buf.length min rem)
      rem - byteBuffer.remaining
    }
  }

  override def read(buf: Array[Byte], off: Int, length: Int): Int = {
    if (logging) log.finest(s"BBIS.read(buf, $off, $length): $byteBuffer ${byteBuffer.remaining()}")
    val rem = byteBuffer.remaining
    if (rem==0) -1 else {
      byteBuffer.get(buf, off, length min rem)
      rem - byteBuffer.remaining
    }
  }

  /** Clear the bytebuffer so it can once again be reused */
  def clear(): Unit = {
    byteBuffer.clear()
  }
}
