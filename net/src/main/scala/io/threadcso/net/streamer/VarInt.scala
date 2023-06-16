package io.threadcso.net.streamer

/**
  *
  * The `varint` encoding defined in the documentation of Google Protocol Buffers can
  * represent signed 64-bit integers (ie Scala `Long` integers). To avoid confusion
  * the type name `VarInt` is defined here to be `Long`. 
  *
  * See `http://code.google.com/apis/protocolbuffers/docs/encoding.html`
  *
  * The unsigned `VarInt` representation of a `Long`
  * consists of a sequence of bytes that toStream a
  * radix-128 integer in littlendian order.  The most
  * significant bit of each byte (`bit7`) is set unless
  * the byte is the last of the sequence.
  *
  * A signed varint `v` is represented as an (unsigned) varint `unsigned` iff:
  * {{{
  *      v == if ((unsigned & 1) !=0 ) ~(unsigned>>>1) else (unsigned>>>1)
  * }}}
  * (where `>>>` means ''sign-extending right shift'')
  */



object VarInt {
  type VarInt = Long

  import java.io.{DataInputStream=>IS, DataOutputStream=>OS}
  private val bit7 = 0x80

  /** Injective coding/decoding support */
  @inline private def eightBits(b: Byte): Long = b & 0xFF

  /** Injective coding/decoding support */
  @inline private def sevenBits(l: Long): Long = l & 0x7F

  /** Injective coding/decoding support */
  @inline private def sevenBitByte(l: Long): Byte = sevenBits(l).asInstanceOf[Byte]

  /** Injective coding/decoding support */
  @inline private def continuedByte(l: Long): Byte = (sevenBits(l) | bit7).asInstanceOf[Byte]

  def readUnsignedVarInt(stream: IS): VarInt = {
    var result: VarInt = 0
    var shift = 0
    var reading = true
    try {
      while (reading) {
        var byte = eightBits(stream.readByte())
        if (byte < bit7) {
          // This is the first `return` I have ever placed in mid-loop!
          // return result | (sevenBits(byte) << shift)
          reading = false
        }
        result = result | (sevenBits(byte) << shift)
        shift += 7
      }
      result
    }
    catch {
      case exn: java.io.EOFException => throw new java.io.EOFException("UnsignedVarInt incompletely represented")
    }
  }

  /** Read a (signed) VarInt */
  def readVarInt(stream: IS): VarInt = {
    var unsigned = readUnsignedVarInt(stream)
    if ((unsigned & 1) != 0) ~(unsigned >>> 1) else (unsigned >>> 1)
  }


  /** Write an unsigned VarInt  */
  def writeUnsignedVarInt(stream: OS, value: VarInt): Unit = {
    var v = value
    var cont = true
    while (cont) {
      val byte = sevenBits(v)
      v = v >>> 7
      cont = v != 0
      stream.writeByte(if (cont) continuedByte(byte) else sevenBitByte(byte))
    }
  }

  /** Write a (signed) VarInt */
  def writeVarInt(stream: OS, value: VarInt): Unit = {
    var bits: VarInt = value << 1
    if (value < 0) bits = ~bits
    writeUnsignedVarInt(stream, bits)
  }

}
