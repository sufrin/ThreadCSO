package org.velvia.msgpack

import java.io.{DataInputStream => DIS, DataOutputStream}
import scala.collection.{Map => CMap}

/**
  *  Codec names changed to be somewhat lighter than those of the original `org.velvia.msgpack`,
  *  but the logic is identical. I gratefully acknowledge the work of velvia.
  */
object CollectionCodecs {
  import Format._

  // NOTE: Arrays automatically get converted to Seqs via WrappedArray
  class `Seq*`[T: Codec] extends Codec[Seq[T]] {
    def pack(out: DataOutputStream, s: Seq[T]): Unit = { packSeq(s, out) }

    val unpackFuncMap = FastByteMap[UnpackFunc](
      MP_ARRAY16 -> { in: DIS => unpackSeq(in.readShort() & MAX_16BIT, in) },
      MP_ARRAY32 -> { in: DIS => unpackSeq(in.readInt(), in) }
    ) ++ (0 to MAX_4BIT).map { len =>
      (MP_FIXARRAY | len).toByte -> { in: DIS => unpackSeq(len, in) }
    }
  }

  class `Map*`[K: Codec, V: Codec] extends Codec[Map[K, V]] {
    private val keyCodec = implicitly[Codec[K]]
    private val valCodec = implicitly[Codec[V]]

    def pack(out: DataOutputStream, m: Map[K, V]): Unit = { packMap(m, out) }

    val unpackFuncMap = FastByteMap[UnpackFunc](
      MP_MAP16 -> { in: DIS => unpackMap(in.readShort() & MAX_16BIT, in)(keyCodec, valCodec) },
      MP_MAP32 -> { in: DIS => unpackMap(in.readInt(), in)(keyCodec, valCodec) }
    ) ++ (0 to MAX_4BIT).map { len =>
      (MP_FIXMAP | len).toByte -> { in: DIS => unpackMap(len, in)(keyCodec, valCodec) }
    }
  }

  class `CMap*`[K: Codec, V: Codec] extends Codec[CMap[K, V]] {
    private val keyCodec = implicitly[Codec[K]]
    private val valCodec = implicitly[Codec[V]]

    def pack(out: DataOutputStream, m: CMap[K, V]): Unit = { packMap(m, out) }

    // Unfortunately have to copy this, maybe can share via trait or something
    val unpackFuncMap = FastByteMap[UnpackFunc](
      MP_MAP16 -> { in: DIS => unpackMap(in.readShort() & MAX_16BIT, in)(keyCodec, valCodec) },
      MP_MAP32 -> { in: DIS => unpackMap(in.readInt(), in)(keyCodec, valCodec) }
    ) ++ (0 to MAX_4BIT).map { len =>
      (MP_FIXMAP | len).toByte -> { in: DIS => unpackMap(len, in)(keyCodec, valCodec) }
    }
  }

  class `Set*`[T: Codec] extends Codec[Set[T]] {
    private val seqCodec = new `Seq*`[T]
    def pack(out: DataOutputStream, s: Set[T]): Unit = {
      seqCodec.pack(out, s.toSeq)
    }
    val unpackFuncMap = seqCodec.unpackFuncMap.mapValues(_.andThen(_.toSet))
  }

}