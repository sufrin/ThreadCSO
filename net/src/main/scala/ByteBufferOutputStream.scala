package ox.net

import java.io.OutputStream
import java.nio.ByteBuffer


/**
  *  An extensible output stream that writes to
  *  a `ByteBuffer`. The buffer increases in
  *  size when necessary to accomodate all that is
  *  written.
  *
  *  TODO: a more subtle enlargement policy
  *  TODO: a switchable allocation policy
  *  TODO: a policy for reclaiming overlong buffers
  *  TODO: bring channel operations onboard (see `UDPChannel`)
  */
class ByteBufferOutputStream(size: Int) extends OutputStream {

  val log = ox.logging.Log("bytebufferoutputstream")
  var buffer: ByteBuffer = ByteBuffer.allocate(size)

  def write(b: Int): Unit = {
    if (buffer.remaining()<1) enlarge()
    buffer.put(b.toByte)
    log.finest(s"BBOS.put($b) $buffer")
  }

  override def write(b: Array[Byte]): Unit = {
    while (buffer.remaining()<b.length) enlarge()
    buffer.put(b)
    log.finest(s"BBOS.put $buffer")
  }

  override def write(b: Array[Byte], off: Int, len: Int): Unit = {
    while (buffer.remaining()<len) enlarge()
    buffer.put(b, off, len)
    log.finest(s"BBOS.put(buf, $off, $len) $buffer")
  }

  def reuse(): Unit = {
    log.finest(s"bbos.reuse($size) / $buffer")
    buffer.clear()
  }

  def enlarge() = {
    val capacity = buffer.capacity * 2
    log.finest(s"BBOS.enlarge -> $capacity")
    val newBuffer = ByteBuffer.allocate(capacity)
    buffer.flip()
    newBuffer.put(buffer)
    buffer = newBuffer
  }

}
