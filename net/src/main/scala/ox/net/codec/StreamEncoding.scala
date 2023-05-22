package ox.net.codec

import scala.reflect.ClassTag

object StreamEncoding {

  import java.io.{DataInputStream, DataOutputStream}

  /** Encoding and decoding of `T` as streams of bytes. */
  trait StreamEncoding[T] {
    /** Stream the datum to the `out` stream */
    def encode(out: DataOutputStream, t: T): Unit

    /** Stream the datum from the `in` stream */
    def decode(in: DataInputStream): T

    /** Signature of the encoding: a byte to support decoding */
    //val signature: Byte
  }

 implicit  object IntStreamEncoding extends StreamEncoding[Int] {
    def encode(out: DataOutputStream, t: Int) = out.writeInt(t)
    def decode(in: DataInputStream): Int = in.readInt()
  }


 implicit  object StringStreamEncoding extends StreamEncoding[String] {
    def encode(out: DataOutputStream, t: String) = out.writeUTF(t)
    def decode(in: DataInputStream): String = in.readUTF()
  }


  class SeqStreamEncoding[T : StreamEncoding](implicit tag: ClassTag[T], encoding: StreamEncoding[T]) extends StreamEncoding[Seq[T]]
  { def encode(out: DataOutputStream, t: Seq[T]): Unit = {
      out.writeInt(t.length)
      for {e <- t} encoding.encode(out, e)
    }

    def decode(in: DataInputStream): Seq[T] = {
      val length = in.readInt()
      val result = Vector.fill[T](length){ encoding.decode(in) }
      result
    }
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

  object IntSeqEncoding extends SeqStreamEncoding[Int]
  object StringSeqEncoding extends SeqStreamEncoding[String]

  object IS extends Tuple2Encoding[Int,String]


}
