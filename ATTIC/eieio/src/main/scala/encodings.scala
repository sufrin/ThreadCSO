package ox.eieio

import java.nio.ByteBuffer
import java.nio.charset.Charset

/**

{{{
        @version 1
        @author Bernard Sufrin, Oxford
        \$Revision: 77 $
        \$Date: 2015-04-29 17:22:38 +0100 (Wed, 29 Apr 2015) $
}}}

        This module exports a handful of coding and decoding functions
        that map specific types to and from their encodings as
        sequences of `Byte` in `ByteBuffer`s. They are used in
        several codec implementations; but codecs are not bound to
        use them.
        
        The directly-useful functions are implementations of the 
        `eieio.types` types:
        
        {{{
          type Decoder[T] = BUFFER => DecodeResult[T]
          type Encoder[T] = (T, BUFFER) => EncodeResult
        }}}
        
        Methods named here named `decode...: DecodeResult[T]` do
        not ''require'' their `BUFFER` arguments to have sufficient
        `remaining` bytes for the given encoding and will return
        `ReadMore` (or `ReadThen`) when there are insufficient.
        They all require as precondition that the buffer is in a
        read-state; and when returning `ReadMore` they ''MUST''
        call `buffer.reset`; and when returning `ReadThen(continuation)`
        they ''MUST NOT''; and the continuation is expected to
        embody that part of the decoder's state that has so far
        been extracted from the buffer.

        A `decode...` method that has a simple header from which
        the length of the representation of the value being decoded
        can be inferred should probably ''not'' attempt to sequester
        the partial result of a ''partial'' decoding in order to
        avoid re-decoding; it's usually simpler to start again.
        Decoders for variable length values delimited (at the end)
        by a particular byte or byte sequence can be built, and for
        efficiency try to avoid rescanning the buffer when more
        bytes have been read. That is the purpose of the
        `ReadThen(continuation)` branch of `DecodeResult`, and
        several examples of its use are defined in the `ox.eieio.codecs`
        object.

        In general methods here named `get...` expect their `BUFFER`
        arguments to have sufficient `remaining` bytes for the given
        encoding. This is not, in general, a reliable assumption.
        Such methods are here simply so I can test my understanding
        of a particular encoding scheme.
  */

object encodings {

  import ox.eieio.types._

  val UTF8: Charset = java.nio.charset.Charset.forName("UTF-8")


  /**
  The `varint` encoding defined in
        the documentation of Google Protocol Buffers can
        represent signed 64-bit integers (ie Scala `Long`
        integers). The name `VarInt` is given to this type
        here in order to avoid any doubt.
        
        See `http://code.google.com/apis/protocolbuffers/docs/encoding.html`

    */
  type VarInt = Long

  /////////////////////////////// ArrayPacket //////////////////////////

  /**
  Decode an `arr: Array[Byte]` wire-encoded as
        `arr.length:[as littlendian byteCount Byte] arr.bytes`.
    */
  def decodeArrayPacket(byteCount: Int, buffer: BUFFER): DecodeResult[Array[Byte]] = {
    buffer.mark
    if (buffer.remaining < byteCount) {
      ReadMore
    }
    else {
      val length = getInt(byteCount, buffer)
      if (buffer.remaining < length) {
        buffer.reset
        ReadMore
      }
      else {
        val bytes = Array.ofDim[Byte](length)
        buffer.get(bytes)
        Decoded(bytes)
      }
    }
  }


  /**
  Encode an `arr: Array[Byte]` as
          `arr.length:[as byteCount Bytes] arr.bytes`
    */
  def encodeArrayPacket(byteCount: Int, bytes: Array[Byte], buffer: BUFFER): BUFFER = {
    val length = bytes.length
    buffer.clear
    putInt(byteCount, length, buffer)
    buffer.put(bytes)
    buffer.flip
  }

  /////////////////////////////// VarIntArrayPacket //////////////////////////


  /**
  Decode an `arr: Array[Byte]` wire-encoded as
        `arr.length:[as VarInt Byte] arr.bytes`
    */
  def decodeVarIntArrayPacket(buffer: BUFFER): DecodeResult[Array[Byte]] = {
    decodeUnsignedVarInt(buffer) match {
      case ReadMore => ReadMore
      case Decoded(result: VarInt) =>
        // now read the bytes {
        val length = result.toInt
        if (buffer.remaining < length) {
          buffer.reset
          ReadMore
        }
        else {
          val bytes = Array.ofDim[Byte](length)
          buffer.get(bytes)
          Decoded(bytes)
        }
      }

  }

  /**
  Encode an `arr: Array[Byte]` as
          `arr.length:[as VarInt Byte] arr.bytes`
    */
  def encodeVarIntArrayPacket(bytes: Array[Byte], buffer: BUFFER): BUFFER = {
    var length = bytes.length
    buffer.clear
    putUnsignedVarInt(length, buffer)
    buffer.put(bytes)
    buffer.flip
  }

  /////////////////////////////// VarInt //////////////////////////

  /**

  The unsigned `VarInt` representation of a `Long`
        consists of a sequence of bytes that encode a
        radix-128 integer in littlendian order.  The most
        significant bit of each byte (`bit7`) is set unless
        the byte is the last of the sequence.

        A signed varint `v` is represented as an (unsigned)
        varint `unsigned` iff:
{{{
        v == if ((unsigned & 1) !=0 ) ~(unsigned>>>1) else (unsigned>>>1)
}}}
        (where `>>>` means ''sign-extending right shift'')

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

  /** Decode an unsigned `VarInt` that may not be completely 
      represented in the current `BUFFER` state. */
  def decodeUnsignedVarInt(buffer: BUFFER): DecodeResult[VarInt] = {
    var result: VarInt = 0
    var shift = 0
    buffer.mark
    while (buffer.remaining > 0) {
      var byte = eightBits(buffer.get)
      if (byte < bit7) {
        return Decoded(result | (sevenBits(byte) << shift))
      }
      result = result | (sevenBits(byte) << shift)
      shift += 7
    }
    // we haven't met a non-continuation byte; ask for more
    buffer.reset
    ReadMore
  }

  /** Decode a (signed) `VarInt` that may not be completely 
      represented in the current `BUFFER` state.  
    */
  def decodeVarInt(buffer: BUFFER): DecodeResult[VarInt] = {
    decodeUnsignedVarInt(buffer) match {
      case ReadMore => ReadMore
      case Decoded(unsigned: VarInt) =>
        Decoded(if ((unsigned & 1) != 0) ~(unsigned >>> 1) else (unsigned >>> 1))
    }
  }


  /**
  Get an unsigned VarInt that is ''completely represented'' in `buffer`
    */
  def getUnsignedVarInt(buffer: BUFFER): VarInt = {
    var result: VarInt = 0
    var shift = 0
    while (buffer.remaining > 0) {
      var byte = eightBits(buffer.get)
      if (byte < bit7) {
        return result | (sevenBits(byte) << shift)
      }
      result = result | (sevenBits(byte) << shift)
      shift += 7
    }
    throw new Error("UnsignedVarInt incompletely represented in buffer")
  }

  /** Get a (signed) VarInt that is ''completely represented'' in `buffer` */
  def getVarInt(buffer: BUFFER): VarInt = {
    var unsigned: VarInt = getUnsignedVarInt(buffer)
    if ((unsigned & 1) != 0) ~(unsigned >>> 1) else (unsigned >>> 1)
  }

  /** Put a ''complete representation'' of an unsigned VarInt into the bufer */
  def putUnsignedVarInt(value: VarInt, buffer: BUFFER): Unit = {
    var v = value
    var cont = true
    while (cont) {
      val byte = sevenBits(v)
      v = v >>> 7
      cont = v != 0
      buffer.put(if (cont) continuedByte(byte) else sevenBitByte(byte))
    }
  }

  /** Put a ''complete representation'' of a (signed) VarInt into the buffer */
  def putVarInt(value: VarInt, buffer: BUFFER): Unit = {
    var bits = value << 1
    if (value < 0) bits = ~bits
    putUnsignedVarInt(bits, buffer)
  }

  /////////////////////////////// Int (littlendian) ///////////////

  /** Get a 4-byte littlendian `Int` 
      
      PRE: `buffer.remaining>=4`
    */
  def getInt(buffer: BUFFER): Int = getInt(4, buffer)

  /** Get a littlendian Int of `size` bytes
      
      PRE: `buffer.remaining>=size`
    */


  @inline def getInt(byteCount: Int, buffer: BUFFER): Int = {
    var result = 0L
    var shift = 0
    for (i <- 0 until byteCount) {
      val b = eightBits(buffer.get) << shift; result = result | b; shift += 8
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


  ///////////////////////////// VarInt Unit Tests /////////////////

  /** Perform simple unit tests on ''complete'' `VarInt` representations.
      (this is so that I can see if I have understood the representation)
    */
  def VarIntUnitTest : Unit = {
    /** round-trip test of unsigned varint encoding */
    def testvw(value: VarInt) = {
      val b = ByteBuffer.allocate(40)
      putUnsignedVarInt(value, b)
      b.flip
      b.mark
      Console.print("%d (%x) ==> ".format(value, value))
      while (b.remaining > 0)
        Console.print("%02x ".format(b.get))
      b.reset
      val d = getUnsignedVarInt(b)
      Console.println(" << (%x)".format(d))
    }

    /** round-trip test of signed varint encoding */
    def testsvw(value: VarInt) = {
      val b = ByteBuffer.allocate(20)
      putVarInt(value, b)
      b.flip
      b.mark
      Console.print("%d (%x) >> ".format(value, value))
      while (b.remaining > 0)
        Console.print("%02x ".format(b.get))
      b.reset
      val d = getVarInt(b)
      Console.println(" << (%x)".format(d))
    }

    testvw(1)
    testvw(300)
    testvw(-1L) // all 1 bits
    testvw(-2L) // not quite ...
    testvw(750)
    testvw(1024)
    testvw(2047)
    testvw(2047 << 20)
    testvw(2047 << 50)
    testvw(1L << 60)
    testvw(2L << 60)
    testvw(4L << 60)

    testsvw(0)
    testsvw(1)
    testsvw(-1)
    testsvw(10)
    testsvw(-10)
    testsvw(100)
    testsvw(-100)
    testsvw(1000)
    testsvw(-1000)
    testsvw(4000)
    testsvw(-4000)
    testsvw(java.lang.Byte.MAX_VALUE)
    testsvw((java.lang.Byte.MIN_VALUE))
    testsvw(java.lang.Short.MAX_VALUE)
    testsvw((java.lang.Short.MIN_VALUE))
    testsvw(java.lang.Integer.MAX_VALUE)
    testsvw((java.lang.Integer.MIN_VALUE))
    testsvw(java.lang.Long.MAX_VALUE / 2)
    testsvw((java.lang.Long.MIN_VALUE / 2))
    testsvw(java.lang.Long.MAX_VALUE)
    testsvw((java.lang.Long.MIN_VALUE))
    testsvw(100000000000L)
    testsvw(-100000000000L)


  }

  /** Run unit tests */
  def main(args: Array[String]) = {
    VarIntUnitTest
  }

}













