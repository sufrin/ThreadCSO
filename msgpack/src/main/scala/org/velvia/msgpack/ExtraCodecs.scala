package org.velvia.msgpack

import java.io.{DataInputStream => DIS, DataOutputStream}
import java.math.{BigDecimal, BigInteger}

/**
  *  Codec names changed to be somewhat lighter than those of the original `org.velvia.msgpack`,
  *  but the logic is identical. I gratefully acknowledge the work of velvia.
  */
object ExtraCodecs {
  import Format._
  import SimpleCodecs._
  import RawStringCodecs.ByteArrayCodec

  // BigDecimals are packed as an array of 2: [scale: Int, unscaledValue: ByteArray]
  implicit object BigDecimalCodec extends Codec[BigDecimal] {
    def pack(out: DataOutputStream, bd: BigDecimal): Unit = {
      out.write(0x2 | MP_FIXARRAY)
      packLong(bd.scale, out)
      packRawBytes(bd.unscaledValue.toByteArray, out)
    }
    val unpackFuncMap = FastByteMap[UnpackFunc](
      (MP_FIXARRAY | 0x2).toByte -> { in: DIS =>
        val scale = IntCodec.unpack(in)
        new BigDecimal(new java.math.BigInteger(ByteArrayCodec.unpack(in)), scale)
      }
    )
  }

  // BigInteger is packed as a byte array
  implicit object BigIntegerCodec extends Codec[BigInteger] {
    def pack(out: DataOutputStream, item: BigInteger): Unit = { ByteArrayCodec.pack(out, item.toByteArray) }
    val unpackFuncMap = ByteArrayCodec.unpackFuncMap.mapValues(_.andThen(new BigInteger(_)))
  }
}