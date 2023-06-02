package ox.net

import java.io.OutputStream
import java.nio.ByteBuffer


/**
  *  An extensible output stream that writes to a `ByteBuffer`. The buffer enlarges
  *  when necessary to accomodate all that is written.
  *
  *  Enlargement is done straightforwardly on the grounds
  *  that if the original estimated size is too small for the largest datagram
  *  to be sent, then the buffer will not take many enlargements before it gets
  *  large enough. A more sophisticated strategy could avoid copying at
  *  the expense of complexity.
  *
  *  For the moment we avoid directly-allocated (off-heap) buffers, because there
  *  is no straightforward API for reclaiming them, and we don't want to  get involved
  *  in the complexities of building direct buffer pools.
  *
  *  The cost of this decision is lowered performance during channel output (the
  *  heap buffer needs to be copied to the kernel's channel buffer. If it becomes
  *  too problematic (in terms of scale) we will think again.
  *
  * @see DatagramOutputStream
  */
object ByteBufferOutputStream extends ox.logging.Log("ByteBufferOutputStream")



class ByteBufferOutputStream(size: Int, deltaCapacity: Int = 100) extends OutputStream  {
  import ByteBufferOutputStream._
  var buffer: ByteBuffer = ByteBuffer.allocate(size)

  def write(b: Int): Unit = {
    if (buffer.remaining()<1) enlarge(buffer.capacity+10)
    buffer.put(b.toByte)
    if (logging) finest(s"BBOS.put($b) $buffer")
  }

  override def write(b: Array[Byte]): Unit = {
    while (buffer.remaining()<b.length) enlarge(b.length)
    buffer.put(b)
    if (logging) finest(s"BBOS.put $buffer")
  }

  override def write(b: Array[Byte], off: Int, len: Int): Unit = {
    while (buffer.remaining()<len) enlarge(len)
    buffer.put(b, off, len)
    if (logging) finest(s"BBOS.put(buf, $off, $len) $buffer")
  }

  /**
    * Enlarge the buffer to `targetCapacity+deltaCapacity`. Invoked by `write` methods
    * when the buffer is too small.
    */
  def enlarge(targetCapacity: Int): Unit = {
    val capacity = targetCapacity + deltaCapacity
    if (logging) finest(s"BBOS.enlarge -> $capacity")
    val newBuffer = ByteBuffer.allocate(capacity)
    buffer.flip()
    newBuffer.put(buffer)
    freeByteBuffer(buffer)
    buffer = newBuffer
  }

  @inline private def freeByteBuffer(buffer: ByteBuffer): Unit = {
    if (buffer.isDirect) {
      //
    }
  }

  override def flush(): Unit = {
    super.flush()
  }

}
