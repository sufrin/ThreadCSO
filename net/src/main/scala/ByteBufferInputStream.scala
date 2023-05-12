package ox.net

import java.io.InputStream
import java.nio.ByteBuffer

object ByteBufferInputStream extends ox.logging.Log("ByteBufferInputStream") {
  def apply(size: Int): ByteBufferInputStream = new ByteBufferInputStream(size)
}

class ByteBufferInputStream(size: Int) extends InputStream {
    import ByteBufferInputStream._
    val buf                    = new Array[Byte](size)
    val byteBuffer: ByteBuffer = ByteBuffer.wrap(buf)

    override def read(): Int = {
      finest(s"BBIS.read(): $byteBuffer ${byteBuffer.remaining()}")
      if (!byteBuffer.hasRemaining) {
        val b =  byteBuffer.get()
        println(s"read=$b")
        b
      } else -1
    }

  override def read(buf: Array[Byte]): Int = {
    finest(s"BBIS.read(buf): $byteBuffer ${byteBuffer.remaining()}")
    val rem = byteBuffer.remaining
    byteBuffer.get(buf, 0, buf.length min rem)
    rem - byteBuffer.remaining
  }

  override def read(buf: Array[Byte], off: Int, length: Int): Int = {
    finest(s"BBIS.read(buf, $off, $length): $byteBuffer ${byteBuffer.remaining()}")
    val rem = byteBuffer.remaining
    byteBuffer.get(buf, off, length min rem)
    rem - byteBuffer.remaining
  }

    def reuse(): Unit = {
      byteBuffer.clear()
    }
}
