package ox.net

import java.io.OutputStream
import java.nio.ByteBuffer


/**
  *  An extensible output stream that writes to
  *  a `ByteBuffer`. The buffer increases in
  *  size when necessary to accomodate all that is
  *  written.
  *
  * @see
  */
object ByteBufferOutputStream extends ox.logging.Log("ByteBufferOutputStream")



class ByteBufferOutputStream(size: Int) extends OutputStream  {
  import ByteBufferOutputStream._
  var buffer: ByteBuffer = ByteBuffer.allocate(size)

  def write(b: Int): Unit = {
    if (buffer.remaining()<1) enlarge()
    buffer.put(b.toByte)
    if (logging) finest(s"BBOS.put($b) $buffer")
  }

  override def write(b: Array[Byte]): Unit = {
    while (buffer.remaining()<b.length) enlarge()
    buffer.put(b)
    if (logging) finest(s"BBOS.put $buffer")
  }

  override def write(b: Array[Byte], off: Int, len: Int): Unit = {
    while (buffer.remaining()<len) enlarge()
    buffer.put(b, off, len)
    if (logging) finest(s"BBOS.put(buf, $off, $len) $buffer")
  }

  def enlarge(): Unit = {
    val capacity = buffer.capacity * 2
    if (logging) finest(s"BBOS.enlarge -> $capacity")
    val newBuffer = ByteBuffer.allocate(capacity)
    buffer.flip()
    newBuffer.put(buffer)
    buffer = newBuffer
  }

  override def flush(): Unit = {
    super.flush()
  }

}
