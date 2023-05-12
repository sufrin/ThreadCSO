
package ox.net
import java.nio.{ByteBuffer => BUFFER}
import java.nio.channels.{ReadableByteChannel, WritableByteChannel}
import java.util.concurrent.atomic.AtomicBoolean

/**
  *  `PseudoChannel`s are wrappers for byte `Channel`s and byte `Stream`s.
  */

trait PseudoChannel {
  def close(): Unit
  def isOpen(): Boolean
  var open: AtomicBoolean = new AtomicBoolean(true)
}

/**
  *   Provides a synchronous interface between a source of buffered
  *   serialized representations of objects, and a `CSO` output channel
  *   that accepts the deserialized representations.
  */
trait ReadablePseudoChannel extends PseudoChannel {
  import io.threadcso._
  import ox.eieio.types._

  def read(dest: BUFFER): Int

  /**
    * A process that reads and decodes serialized representations
    * of objects, and writes them to `out`.
    *
    * The process terminates when `read` yields `-1`, or when `out` is
    * (discovered to be) closed (during an output).
    *
    */
  def CopyFromNet[T](out: !![T], decoder: Decoder[T]): PROC = {
    val copy = proc(this.toString + ".CopyFromNet") {
      PseudoChannel.log.fine("Starting CopyFromNet")
      var decode: () => DecodeResult[T] = decoder.decode _
      val buffer = decoder.buffer
      var lastResult = null.asInstanceOf[T]

      // DONE: removed the recursion
      def processBuffer(size: Int): Unit = {
        PseudoChannel.log.finer(s"processBuffer(${size}) [${buffer}]")
        var processing = size > 0
        if (processing) buffer.flip
        while (processing) {
          var portOpen = true
          while (open.get && portOpen && buffer.remaining > 0) {
            buffer.mark // in case of a ReadMore
            decode() match {
              case ReadMore =>
                // there's not enough to decode
                // compact what's present and read some more
              {
                PseudoChannel.log.finest("Reading more(%s)".format(buffer))
                buffer.compact
                PseudoChannel.log.finest("Reading more compacted(%s)".format(buffer))
                processing = (0 < read(buffer))
                if (processing) buffer.flip
              }
              case ReadThen(cont) =>
                // there was not enough to decode
                // the decoder has already repositioned the buffer
                // by compacting the partly-decoded prefix
              {
                PseudoChannel.log.finest("ReadThen (%s)".format(buffer))
                // switch the decoder to the continuation for the next read
                decode = cont
                processing = (0 < read(buffer))
                if (processing) buffer.flip
              }
              case Decoded(res: T@unchecked) =>
                // decoding was successful
                // there may be (several) more decodeables in the buffer
                // we don't compact at this stage
              {
                portOpen = attempt {
                  PseudoChannel.log.finest("Decoded (%s)".format(res))
                  lastResult = res
                  out ! res
                  true
                } {
                  false
                }
                // switch the decoder back to the original
                decode = decoder.decode _
              }
              case Completed(stillOpen: Boolean) =>
                // decoding was successful
                // the datum has been disposed of by the decoder
                // there may be (several) more decodeables in the buffer
                // the decoder may have decided to close the port
              {
                portOpen = stillOpen
              }
            }
          }
          // not portOpen || buffer.remaining==0
          if (portOpen && out.canOutput) { // continue issuing read requests
            PseudoChannel.log.finest("Restarting after (%s)\n::: %s".format(lastResult, buffer))
            buffer.compact
            PseudoChannel.log.finest("Compacted after (%s)\n::: %s".format(lastResult, buffer))
            processing = (open.get && 0 < read(buffer))
            if (processing) buffer.flip
          }
          else {
            // the output side of the peer's channel was closed by the peer
            // shut down any input processing at this end
            PseudoChannel.log.fine("Peer output port closed; shutting down input socket")
            buffer.clear
            out.closeOut()
            close()
            processing = false
          }
        }
        //
        {
          PseudoChannel.log.fine("Peer socket closed; shutting down output port")
          buffer.clear
          out.closeOut()
          close()
          ()
        }
      }
      // start the processing loop
      processBuffer(read(buffer))
    }
    copy
  }

}

/**
  *   Provides a synchronous encoding interface between a `CSO` input port
  *   that provides objects on demand, and a sink for their buffered
  *   serialized representations.
  */

trait WritablePseudoChannel extends PseudoChannel {
  import io.threadcso._
  import ox.eieio.types._

  def write(source: BUFFER): Int

  /**
    * A process that repeatedly reads objects of type `T` from `in`, and
    * serializes them using `encoder` and `write`s their buffered serializations.
    *
    * The process terminates when `in` is (discovered to be) closed, or
    * `write` signifies that it can no longer accept buffers by returning
    * a nonpositive result.
    *
    */
  def CopyToNet[T](in: ??[T], encoder: Encoder[T]): PROC = {
    val worker =
      proc(this.toString + ".CopyToNet") {
        PseudoChannel.log.fine("Starting CopyToNet")
        val buffers  = encoder.buffers
        val nbuffers = buffers.length
        PseudoChannel.log.fine("Starting CopyToNet")
        repeat {
          val value = in ? ()
          encoder.clear()
          encoder.encode(value)
          var index = 0
          while (index != nbuffers) {
            val size = write(buffers(index))
            if (size > 0) {
              while (index < nbuffers && buffers(index).remaining == 0) {
                index += 1
              }
            } else {
              PseudoChannel.log.fine("Peer output socket closed; shutting down input port")
              in.closeIn()
            }
          }
        }
        PseudoChannel.log.fine("Peer input port closed; shutting down PseudoChannel")
        close()
      }
      worker
  }
}


object PseudoChannel {
  val log = ox.logging.Log("PseudoChannel")

  /**  A channel formed from the given `InputStream` */
  def apply(stream: java.io.InputStream): ReadablePseudoChannel = new InputStreamChannel(stream)

  /**  A channel formed from the given `ReadableByteChannel` */
  def apply(channel: ReadableByteChannel): ReadablePseudoChannel = new ReadablePseudoChannel {
    override def read(dest: BUFFER): Int = channel.read(dest)
    override def close(): Unit = channel.close()
    override def isOpen(): Boolean = channel.isOpen()
  }

  /** A channel formed from the given `OutputStream` */
  def apply(stream: java.io.OutputStream): WritablePseudoChannel = new OutputStreamChannel(stream)

  /**  A channel formed from the given `WritableByteChannel` [sic] */
  def apply(channel: WritableByteChannel): WritablePseudoChannel = new WritablePseudoChannel {
    override def write(dest: BUFFER): Int = channel.write(dest)
    override def close(): Unit = channel.close()
    override def isOpen(): Boolean = channel.isOpen()
  }
}

/**
  * Efficiency can be improved by using backing arrays if any  */
class OutputStreamChannel(stream: java.io.OutputStream) extends WritablePseudoChannel {

  def close(): Unit = {
    PseudoChannel.log.finest(s"OutputStream.Close($open)")
    if (open.getAndSet(false)) { stream.close() }
  }

  def isOpen(): Boolean = open.get

  def write(source: BUFFER): Int = {
    //PseudoChannel.log.log.finest(s"write")
    var count = 0
    while (open.get && source.remaining() > 0) {
      val b = source.get()
      //PseudoChannel.log.log.finest(s"writing '${b.toChar}'")
      stream.write(b)
      count += 1
    }
    stream.flush()
    PseudoChannel.log.finest(s"write=$count")
    count
  }
}

/** Efficiency can be improved by using backing arrays if any */
class InputStreamChannel(stream: java.io.InputStream) extends ReadablePseudoChannel {

  def close(): Unit =  {
    PseudoChannel.log.finest(s"InputStream.Close($open)")
    if (open.getAndSet(false)) { stream.close() }
    }

    def isOpen(): Boolean = open.get

    def read(dest: BUFFER): Int = {
        PseudoChannel.log.finest("read()")
        var count = 0
        try {
          @inline def readOne() = {
              val b = stream.read()
              b match {
                case -1 => open.set(false)
                case _ =>
                  dest.put(b.asInstanceOf[Byte])
                  count += 1
              }
          }
          if ( dest.remaining()>0 && open.get ) {
              readOne() // force block if necessary
              while (open.get && stream.available()>0) readOne()
          }
        } catch {
          case exn: java.net.SocketException =>
            PseudoChannel.log.finest(s"read throws $exn, closing")
            close()
        }
        PseudoChannel.log.finest(s"read=$count")
        if (open.get) count else -1
      }
}
