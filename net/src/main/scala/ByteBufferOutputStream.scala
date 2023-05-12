package ox.net

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer

class ByteBufferOutputStream(size: Int) extends ByteArrayOutputStream(size) {
  lazy val buffer: ByteBuffer = ByteBuffer.wrap(buf)

  def reuse(): Unit = {
    super.reset()
    buffer.position(0)
    buffer.limit(buf.length)
  }
}
