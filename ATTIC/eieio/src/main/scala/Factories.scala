package ox.eieio
import ox.eieio.codecs.UTF8
import ox.eieio.types._
import ox.logging.Log

import java.io.{BufferedReader, EOFException, InputStream, OutputStream}
import java.nio.ByteBuffer
import java.nio.channels.{ByteChannel, SocketChannel}

object EncodingUtilities {
  type VarInt = Long

  /**
    *
    * The unsigned `VarInt` representation of a `Long`
    * consists of a sequence of bytes that encode a
    * radix-128 integer in littlendian order.  The most
    * significant bit of each byte (`bit7`) is set unless
    * the byte is the last of the sequence.
    *
    * A signed varint `v` is represented as an (unsigned)
    * varint `unsigned` iff:
    * {{{
    *v == if ((unsigned & 1) !=0 ) ~(unsigned>>>1) else (unsigned>>>1)
    * }}}
    * (where `>>>` means ''sign-extending right shift'')
    *
    */

  private val bit7 = 0x80

  /** Injective coding/decoding support */
  @inline private def eightBits(b: Byte): Long = b & 0xFF

  /** Injective coding/decoding support */
  @inline private def sevenBits(l: Long): Long = l & 0x7F

  /** Injective coding/decoding support */
  @inline private def sevenBitByte(l: Long): Byte = sevenBits(l).asInstanceOf[Byte]

  /** Injective coding/decoding support */
  @inline private def continuedByte(l: Long): Byte = (sevenBits(l) | bit7).asInstanceOf[Byte]


  /** Put a ''complete representation'' of an unsigned VarInt into the bufer */
  def putUnsignedVarInt(value: VarInt, out: OutputStream): Unit = {
    var v = value
    var cont = true
    while (cont) {
      val byte = sevenBits(v)
      v = v >>> 7
      cont = v != 0
      out.write(if (cont) continuedByte(byte) else sevenBitByte(byte))
    }
  }

  /** Put a ''complete representation'' of a (signed) VarInt into the buffer */
  def putVarInt(value: VarInt, out: OutputStream): Unit = {
    var bits = value << 1
    if (value < 0) bits = ~bits
    putUnsignedVarInt(bits, out)
  }

  /**
    * Get an unsigned VarInt that is ''completely represented'' as a prefix of `in`
    */
  def getUnsignedVarInt(in: InputStream): VarInt = {
    var result: VarInt = 0
    var shift = 0
    var going = true
    var ready = false
    while (going && !ready) {
      in.read() match {
        case -1  => going = false
        case b =>
          val byte = eightBits (b.toByte)
          if (byte < bit7) ready = true
          result = result | (sevenBits (byte) << shift)
          shift += 7
      }
    }
    // !going || ready
    if (ready)
      result
    else
      throw new Error("UnsignedVarInt incompletely represented")
  }

  /** Get a (signed) VarInt that is ''completely represented'' as a prefix of  `in` */
  def getVarInt(in: InputStream): VarInt = {
    var unsigned: VarInt = getUnsignedVarInt(in)
    if ((unsigned & 1) != 0) ~(unsigned >>> 1) else (unsigned >>> 1)
  }


  /////////////////////////////// Int (littlendian) ///////////////

  /** Get a 4-byte littlendian `Int` from `in`
    ** PRE: `buffer.remaining>=4`
    */
  def getInt(in: InputStream): Int = getInt(4, in)

  /**
    *  Get a littlendian Int of `size` bytes that prefixes `in`
    */
  @inline def getInt(byteCount: Int, in: InputStream): Int = {
    var result = 0L
    var shift = 0
    for (i <- 0 until byteCount) {
      in.read() match {
        case -1   =>
          throw new Error("$byteCount byte Int incompletely represented")
        case byte =>
          val b = eightBits(byte.toByte) << shift
          result = result | b
          shift += 8
      }
    }
    result.asInstanceOf[Int]
  }

  /** Put the `byteCount`-bytes encoded littlendian `_value: Int` */
  @inline def putInt(byteCount: Int, _value: Int, buffer: BUFFER): Unit = {
    var value = _value
    for (i <- 0 until byteCount) {
      val b = (value & 0xFF)
      buffer.put(b.asInstanceOf[Byte])
      value = value >>> 8
    }
  }

}

object Factories {

  /**
    * An encoding is (implicitly) associated with an output
    * channel that is capable of outputting `ByteBuffers`
    */

  trait Encoding[T] {
    /**
      *
      * @param value the value to encode, and output
      * @return the iterable sequence of buffers (possibly empty) to be output.
      *         The iterator is empty only if the encoding has already been
      *         written to the associated output channel.
      */
    def encode(value: T): Iterable[BUFFER]

    /**
      * Reset the state of this encoder, if necessary.
      */
    def clear(): Unit
  }

  /**
    * A decoding is implicitly associated with an input source (usually a `ByteChannel`)
    * from which successive representations of `T` can be read.
    */

    trait Decoding[T] {
    /**
      * Decode and return the next available representation of a [T]
      * (if any) from the input source; otherwise `null`. If `!canDecode` then
      * not only did `decode` return `null`, but all subsequent invocation
      * will return `null`.
      */
    def decode(): T

    /**
      * @return false iff the previous (and all subsequent) results of `decode()`
      *         (will) yield `null`.
      */
    def canDecode: Boolean
  }

  trait EncoderFactory[OUT] {
    /** Construct an encoder (associated with `chan`) for `OUT`*/
    def makeEncoder(chan: ByteChannel): Encoding[OUT]

    /** Set socket options relevant to the encoder */
    def setOptions(chan: ByteChannel): Unit
  }

  trait DecoderFactory[IN] {
    /** Construct an decoder for `IN` with buffers suitable for use with `chan` */
    def makeDecoder(chan: ByteChannel): Decoding[IN]

    /** Set socket options relevant to the decoder */
    def setOptions(chan: ByteChannel): Unit
  }

  /** writes `[length: Int, bytes]` */
  object IntPacketOutFactory extends EncoderFactory[String] {
    val SND = 16000

    import ox.eieio.options._

    /** Refactor */
    override def setOptions(channel: ByteChannel): Unit =
    channel match {
      case channel: SocketChannel =>
        setOption(channel, SO_SNDBUF, SND)
        setOption(channel, TCP_NODELAY, true) // TODO: factory parameter
        setOption(channel, SO_KEEPALIVE, true)
      case _ =>
    }

    override def makeEncoder(chan: ByteChannel): Encoding[String] = new Encoding[String] {
      private val byteCount = ByteBuffer.wrap(Array[Byte](0, 0, 0, 0))

      override def encode(value: String): Iterable[BUFFER] = {
        val bytes = value.getBytes(UTF8)
        encodings.putInt(4, bytes.length, byteCount)
        List(
          byteCount,
          ByteBuffer.wrap(bytes)
        )
      }

      override def clear(): Unit = byteCount.clear()
    }
  }

  /** writes `[length: Int, bytes]` */
  object VarIntPacketOutFactory extends EncoderFactory[String] {
    val SND = 16000

    import ox.eieio.options._

    /** Refactor */
    override def setOptions(channel: ByteChannel): Unit =
      channel match {
        case channel: SocketChannel =>
          setOption(channel, SO_SNDBUF, SND)
          setOption(channel, TCP_NODELAY, true) // TODO: factory parameter
          setOption(channel, SO_KEEPALIVE, true)
        case _ =>
      }

    override def makeEncoder(chan: ByteChannel): Encoding[String] = new Encoding[String] {
      private val out = java.nio.channels.Channels.newOutputStream(chan)

      override def encode(value: String): Iterable[BUFFER] = {
        import EncodingUtilities._
        val bytes = value.getBytes(UTF8)
        putUnsignedVarInt(bytes.length, out)
        out.write(bytes)
        out.flush()
        Nil
      }

      override def clear(): Unit = ()
    }
  }

  object VarIntPacketInFactory extends DecoderFactory[String] {
    val RCV = 16000

    import ox.eieio.options._

    /** Refactor */
    override def setOptions(channel: ByteChannel): Unit = channel match {
      case channel: SocketChannel =>
        setOption(channel, SO_RCVBUF, RCV) // TODO: factory parameters
        setOption(channel, TCP_NODELAY, true)
        setOption(channel, SO_KEEPALIVE, true)
      case _ =>
    }

    override def makeDecoder(chan: ByteChannel): Decoding[String] = new Decoding[String] {

      import EncodingUtilities._

      var open = true
      val bytes = java.nio.channels.Channels.newInputStream(chan)

      override def decode(): String = {
        val count = getUnsignedVarInt(bytes).toInt
        new String(bytes.readNBytes(count), UTF8)
      }

      /**
        * @return false iff the previous (and all subsequent) results of `decode()`
        *         (will) yield `null`.
        */
      override def canDecode: Boolean = ???
    }
  }

  /** writes `[length: Int, bytes]` */
  object IntPacketInFactory extends DecoderFactory[String] {
    val RCV = 16000

    import ox.eieio.options._

    /** Refactor */
    override def setOptions(channel: ByteChannel): Unit = channel match {
      case channel: SocketChannel =>
        setOption(channel, SO_RCVBUF, RCV) // TODO: factory parameters
        setOption(channel, TCP_NODELAY, true)
        setOption(channel, SO_KEEPALIVE, true)
      case _ =>
    }

    override def makeDecoder(chan: ByteChannel): Decoding[String] = new Decoding[String] {
      import EncodingUtilities._
      var open = true
      val bytes = java.nio.channels.Channels.newInputStream(chan)

      override def decode(): String = {
        val count  = getInt(bytes)
        new String(bytes.readNBytes(count), UTF8)
      }

      /**
        * @return false iff the previous (and all subsequent) results of `decode()`
        *         (will) yield `null`.
        */
      override def canDecode: Boolean = ???
    }
  }

  object UTF8OutFactory extends EncoderFactory[String] {
    val SND = 16000

    import ox.eieio.options._

    /** Refactor */
    override def setOptions(channel: ByteChannel): Unit =
      channel match {
        case channel: SocketChannel =>
          setOption(channel, SO_SNDBUF, SND)
          setOption(channel, TCP_NODELAY, true) // TODO: factory parameter
          setOption(channel, SO_KEEPALIVE, true)
        case _ =>
      }

    override def makeEncoder(chan: ByteChannel): Encoding[String] = new Encoding[String] {
      val data = new java.io.DataOutputStream(java.nio.channels.Channels.newOutputStream(chan))
      var open: Boolean = true
      override def encode(value: String): Iterable[BUFFER] = {
        data.writeUTF(value)
        data.flush()
        Nil
      }

      override def clear(): Unit = ()
    }
  }

  object UTF8InFactory extends DecoderFactory[String] {
    val log = Log("crlf")
    val RCV = 16000

    import ox.eieio.options._

    override def setOptions(channel: ByteChannel): Unit = channel match {
      case channel: SocketChannel =>
        setOption(channel, SO_RCVBUF, RCV) // TODO: factory parameters
        setOption(channel, TCP_NODELAY, true)
        setOption(channel, SO_KEEPALIVE, true)
      case _ =>
    }

    override def makeDecoder(chan: ByteChannel): Decoding[String] = new Decoding[String] {
      val data = new java.io.DataInputStream(java.nio.channels.Channels.newInputStream(chan))
      var open: Boolean = true

      override def canDecode: Boolean = open

      def decode(): String = try data.readUTF() catch {
        case exn: EOFException => open = false; null
      }
    }
  }

  object crlfInFactory extends DecoderFactory[String] {
    val log = Log("crlf")
    val RCV = 16000

    import ox.eieio.options._

    override def setOptions(channel: ByteChannel): Unit = channel match {
      case channel: SocketChannel =>
        setOption(channel, SO_RCVBUF, RCV) // TODO: factory parameters
        setOption(channel, TCP_NODELAY, true)
        setOption(channel, SO_KEEPALIVE, true)
      case _ =>
    }

    override def makeDecoder(chan: ByteChannel): Decoding[String] = new Decoding[String] {
      val chars =  new BufferedReader(java.nio.channels.Channels.newReader(chan, UTF8.newDecoder(), 8192)) // TODO: factory parameters
      var open: Boolean = true
      override def canDecode: Boolean = open
      def decode(): String = chars.readLine() match {
        case null   => open=false; ""
        case string => string
      }
    }
  }

  object crlfOutFactory extends EncoderFactory[String] {
    val SND = 16000

    import ox.eieio.options._

    override def setOptions(channel: ByteChannel): Unit =
      channel match {
        case channel: SocketChannel =>
          setOption(channel, SO_SNDBUF, SND)
          setOption(channel, TCP_NODELAY, true) // TODO: factory parameter
          setOption(channel, SO_KEEPALIVE, true)
        case _ =>
      }


    val useJava = true
    val synchronous = true

    override def makeEncoder(chan: ByteChannel): Encoding[String] =
      if (useJava)
        new Encoding[String] {
          val chars = java.nio.channels.Channels.newWriter(chan, UTF8.newEncoder(), 8192) // TODO: factory parameters
          private val crlf = "\r\n"
          var open: Boolean = true
          override def encode(value: String): Iterable[BUFFER] = {
            chars.write(value)
            chars.write(crlf)
            if (synchronous) chars.flush()
            Nil
          }
          override def clear(): Unit = ()
        }
      else
        new Encoding[String] {
          private val crlf = ByteBuffer.wrap(Array[Byte]('\r', '\n'))

          override def encode(value: String): Iterable[BUFFER] = List(
            ByteBuffer.wrap(value.getBytes(UTF8)),
            crlf
          )

          override def clear(): Unit = crlf.clear()
      }
  }

}
