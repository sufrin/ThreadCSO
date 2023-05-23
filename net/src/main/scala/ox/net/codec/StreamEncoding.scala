package ox.net.codec

import ox.net.codec.VarInt.VarInt

import scala.reflect.ClassTag


/**
  *  Vestigial machinery to help roll-your-own stream encodings.
  *
  *  The definitions here are just examples of how to make a
  *  straightforward encoding that doesn't rely too much on
  *  the typeclass machinery.
  *
  *  They are not recommended for general use :out of the box"
  *  since the `msgpack` machinery delivers more compact
  *  streams -- albeit at some cost in compressing/decompressing.
  *
  * @see ox.net.GenericChannelFactory, org.velvia.MessagePack, ox.net.VarInt
  */
object StreamEncoding {

  import java.io.{DataInputStream, DataOutputStream}

  /** Encoding and decoding of `T` as streams of bytes. */
  trait StreamEncoding[T] {
    /** Stream the datum to the `out` stream */
    def encode(out: DataOutputStream, t: T): Unit

    /** Stream the next datum from the `in` stream */
    def decode(in: DataInputStream): T
  }

 implicit  object IntEncoding extends StreamEncoding[Int] {
    def encode(out: DataOutputStream, t: Int) = out.writeInt(t)
    def decode(in: DataInputStream): Int = in.readInt()
  }

  implicit object VarIntEncoding extends StreamEncoding[VarInt] {
    def encode(out: DataOutputStream, t: VarInt) = ox.net.codec.VarInt.writeVarInt(out, t)
    def decode(in: DataInputStream): VarInt = ox.net.codec.VarInt.readVarInt(in)
  }


 implicit  object StringEncoding extends StreamEncoding[String] {
    def encode(out: DataOutputStream, t: String) = out.writeUTF(t)
    def decode(in: DataInputStream): String = in.readUTF()
  }


  class SeqEncoding[T : StreamEncoding](implicit tag: ClassTag[T], encoding: StreamEncoding[T]) extends StreamEncoding[Seq[T]]
  {
    def encode(out: DataOutputStream, t: Seq[T]): Unit = {
      out.writeInt(t.length)
      for {e <- t} encoding.encode(out, e)
    }

    def decode(in: DataInputStream): Seq[T] = {
      val length = in.readInt()
      val result = Vector.fill[T](length){ encoding.decode(in) }
      result
    }
  }

  class Case2Encoding[K, T: StreamEncoding, U: StreamEncoding](apply: (T,U)=>K, unapply: K=>Option[(T,U)]) extends StreamEncoding[K] {
    val encoding = new Tuple2Encoding[T,U]

    /** Stream the datum to the `out` stream */
    def encode(out: DataOutputStream, t: K): Unit = encoding.encode(out, unapply(t).get)

    /** Stream the next datum from the `in` stream */
    def decode(in: DataInputStream): K = { val (t,u) = encoding.decode(in); apply(t,u) }
  }

  class Tuple2Encoding[T : StreamEncoding, U : StreamEncoding](implicit enc1: StreamEncoding[T], enc2: StreamEncoding[U]) extends StreamEncoding[(T, U)] {
    def encode(out: DataOutputStream, v: (T, U)): Unit = {
      enc1.encode(out, v._1)
      enc2.encode(out, v._2)
    }

    def decode(in: DataInputStream): (T, U) = {
      val t = enc1.decode(in)
      val u = enc2.decode(in)
      (t, u)
    }
  }

  class Tuple3Encoding[T: StreamEncoding, U: StreamEncoding, V: StreamEncoding](implicit enc1: StreamEncoding[T], enc2: StreamEncoding[U], enc3: StreamEncoding[V]) extends StreamEncoding[(T, U, V)] {
    def encode(out: DataOutputStream, v: (T, U, V)): Unit = {
      enc1.encode(out, v._1)
      enc2.encode(out, v._2)
      enc3.encode(out, v._3)
    }

    def decode(in: DataInputStream): (T, U, V) = {
      val t = enc1.decode(in)
      val u = enc2.decode(in)
      val v = enc3.decode(in)
      (t, u, v)
    }
  }

  class Tuple4Encoding[T: StreamEncoding, U: StreamEncoding, V: StreamEncoding, W: StreamEncoding](implicit enc1: StreamEncoding[T], enc2: StreamEncoding[U], enc3: StreamEncoding[V], enc4: StreamEncoding[W])
    extends StreamEncoding[(T, U, V, W)] {
    def encode(out: DataOutputStream, v: (T, U, V, W)): Unit = {
      enc1.encode(out, v._1)
      enc2.encode(out, v._2)
      enc3.encode(out, v._3)
      enc4.encode(out, v._4)
    }

    def decode(in: DataInputStream): (T, U, V, W) = {
      val t = enc1.decode(in)
      val u = enc2.decode(in)
      val v = enc3.decode(in)
      val w = enc4.decode(in)
      (t, u, v, w)
    }
  }

  object IntSeqEncoding extends SeqEncoding[Int]
  object StringSeqEncoding extends SeqEncoding[String]
  object IS extends Tuple2Encoding[Int,String]


}
