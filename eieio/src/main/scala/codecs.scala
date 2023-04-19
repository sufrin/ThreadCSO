package ox.eieio

/**
{{{
        @version 1
        @author Bernard Sufrin, Oxford
        \$Revision: 54 $
        \$Date: 2015-03-19 20:32:17 +0000 (Thu, 19 Mar 2015) $
}}}

        Some useful simple encoder/decoder pairs.

        `IntPacketString` and `VarIntPacketString` use `eieio.encodings`
        for their stock representations of types `Int`, (unsigned)
        `VarInt`; and ''packetised'' representations of arrays of
        bytes that prefix the bytes on the wire with the size of
        the array.

        `StringCRLF`, `StringLF`, and `StringNUL` demonstrate how
        to use `ReadThen(continuation)` as a way of avoiding the
        re-parsing of wire encodings when a buffer doesn't initially
        contain enough information to predict the size of the
        representation that is arriving. 
  */

object codecs {

  import ox.eieio.encodings._
  import ox.eieio.types._

  val UTF8 = java.nio.charset.Charset.forName("UTF-8")

  /** `[size: Int; UTF8 Bytes]` */
  object IntPacketString extends SimpleCodec[String] {
    def Encoder(bufferSize: Int) = new Encoder(bufferSize)

    def Decoder(bufferSize: Int) = new Decoder(bufferSize)

    class Encoder(bufferSize: Int) extends SimpleEncoder[String](bufferSize) {
      def encode(string: String): Unit = {
        val bytes = string.getBytes(UTF8)
        encodeArrayPacket(4, bytes, buffer)
      }
    }

    class Decoder(bufferSize: Int) extends SimpleDecoder[String](bufferSize) {
      def decode(): DecodeResult[String] = {
        decodeArrayPacket(4, buffer) match {
          case ReadMore => ReadMore
          case Decoded(array: Array[Byte]) => Decoded(new String(array, UTF8))
        }
      }
    }

  }

  /** `[size: VarInt; UTF Bytes]` */
  object VarIntPacketString extends SimpleCodec[String] {
    def Encoder(bufferSize: Int) = new Encoder(bufferSize)

    def Decoder(bufferSize: Int) = new Decoder(bufferSize)

    class Encoder(bufferSize: Int) extends SimpleEncoder[String](bufferSize) {
      def encode(string: String) : Unit = {
        val bytes = string.getBytes(UTF8)
        var length = bytes.length
        buffer.clear()
        putUnsignedVarInt(length, buffer)
        buffer.put(bytes)
        buffer.flip()
      }
    }

    class Decoder(bufferSize: Int) extends SimpleDecoder[String](bufferSize) {
      def decode(): DecodeResult[String] = {
        decodeVarIntArrayPacket(buffer) match {
          case ReadMore => ReadMore
          case Decoded(array: Array[Byte]) => Decoded(new String(array, UTF8))
        }
      }
    }

  }

  /** `[UTF8 Bytes; delim]` -- there must be no `delim` in the string unless
       it is acceptable for a single encoded string to be decoded
       as if it were several strings.
    */
  class DelimitedStringCodec(delim: Char) extends SimpleCodec[String] {
    def Encoder(bufferSize: Int) = new Encoder(bufferSize)

    def Decoder(bufferSize: Int) = new Decoder(bufferSize)

    class Encoder(bufferSize: Int) extends SimpleEncoder[String](bufferSize) {
      def encode(string: String) : Unit = {
        val bytes = string.getBytes(UTF8)
        var length = bytes.length
        buffer.clear
        buffer.put(bytes)
        buffer.put(delim.asInstanceOf[Byte])
        buffer.flip
      }
    }


    class Decoder(bufferSize: Int) extends SimpleDecoder[String](bufferSize) {
      // val log   = new Logger("DelimString")
      // var conts = 0
      def decode(): DecodeResult[String] = {
        buffer.mark
        decoder(0, false)
      }

      def decoder(_length: Int, continuation: Boolean): DecodeResult[String] = {
        // Reset the state of the decoder
        var length = _length
        if (continuation) {
          buffer.position(length)
        }
        // ------------------------------
        while (buffer.remaining > 0) {
          if (buffer.get == delim) {
            val array = Array.ofDim[Byte](length)
            if (continuation) buffer.position(0) else buffer.reset
            buffer.get(array) // the body
            buffer.get // but not the newline
            val res = new String(array, UTF8)
            return Decoded(res)
          }
          else {
            length = length + 1
          }
        }
        // This avoids reparsing strings that span network packets
        {
          buffer.position(buffer.limit - length) // we want to keep length bytes
          buffer.compact() // in the compacted buffer
          buffer.position(length) // and start reading from there
          // conts+=1; log.finer("%d => ReadThen(%d): %s".format(conts, length, buffer))
          ReadThen(() => decoder(length, true))
        }
      }
    }

  }

  /** `[UTF8 Bytes; LF]` -- there must be no `LF` in the string */
  object LFString extends DelimitedStringCodec('\n') {}


  /** `[UTF8 Bytes; NUL]` -- there must be no `NUL` in the string */
  object NULString extends DelimitedStringCodec('\u0000') {}


  /** `[UTF8 Bytes; CRLF]` -- there must be no CRLF in the string */
  object CRLFString extends SimpleCodec[String] {
    def Encoder(bufferSize: Int) = new Encoder(bufferSize)

    def Decoder(bufferSize: Int) = new Decoder(bufferSize)

    class Encoder(bufferSize: Int) extends SimpleEncoder[String](bufferSize) {
      def encode(string: String) : Unit = {
        val bytes = string.getBytes(UTF8)
        var length = bytes.length
        buffer.clear
        buffer.put(bytes)
        buffer.put('\r'.asInstanceOf[Byte])
        buffer.put('\n'.asInstanceOf[Byte])
        buffer.flip
      }
    }

    class Decoder(bufferSize: Int) extends SimpleDecoder[String](bufferSize) {
      def decode(): DecodeResult[String] = {
        buffer.mark
        decoder(0, false, false)
      }

      def decoder(_length: Int, _cr: Boolean, continuation: Boolean): DecodeResult[String] = {
        // Reset the state of the decoder
        var length = _length
        var cr = _cr
        if (continuation) {
          buffer.position(length)
        }
        // ------------------------------
        var ch: Byte = 0

        while (buffer.remaining > 0) {
          ch = buffer.get
          if (ch == '\n') {
            if (cr) length -= 1
            val array = Array.ofDim[Byte](length)
            if (continuation) buffer.position(0) else buffer.reset
            buffer.get(array) // the body
            buffer.get // but not the newline
            if (cr) buffer.get // or the cr
            return Decoded(new String(array, UTF8))
          }
          else {
            length += 1
            cr = ch == '\r'
          }
        }
        // this avoids reparsing of strings that span network packets
        {
          buffer.position(buffer.limit - length)
          buffer.compact()
          buffer.position(length)
          ReadThen(() => decoder(length, cr, true))
        }
      }
    }

    /** For comparison: this is a CRLF string `decode` implementation that doesn't
         use the `ReadThen(continuation)` decode result protocol. 
      */
    def nonContDecode(buffer: BUFFER): DecodeResult[String] = {
      buffer.mark
      var length = 0
      var cr = false // <=> previous ch was '\r'
      var ch: Byte = 0
      while (buffer.remaining > 0) {
        ch = buffer.get
        if (ch == '\n') {
          if (cr) length -= 1
          val array = Array.ofDim[Byte](length)
          buffer.reset
          buffer.get(array) // the body
          buffer.get // but not the newline
          if (cr) buffer.get // or the cr
          return Decoded(new String(array, UTF8))
        }
        else {
          length += 1
          cr = ch == '\r'
        }
      }
      buffer.reset
      ReadMore
    }


  }

}







