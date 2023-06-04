package ox.net.codec

import ox.net.codec.VarInt._


/**
  *  Machinery to help roll-your-own stream encodings for use on
  *  cross-network transport or for "pickling" values in
  *  persistent storage.
  *
  *  The definitions here exemplify how to make a systematic and straightforward
  *  encoding that doesn't rely too much on the typeclass machinery.
  *
  *  Warning/Advertisement:
  *  The `msgpack` machinery delivers considerably more compact streams -- albeit at some
  *  cost in complexity, and in computing the compressed representation. But its killer
  *  advantage is interoperability there are many language bindings for `msgpack`.
  *
  * @see ox.net.VelviaChannelFactory, org.velvia.MessagePack
  */
object DataStreamEncoding {

  import java.io.{DataInputStream, DataOutputStream}

  /** Encoding and decoding of `T` as streams of bytes. */
  trait Stream[T] {
    /** Stream the datum to the `out` stream */
    def encode(out: DataOutputStream, t: T): Unit

    /** Stream the next datum from the `in` stream */
    def decode(in: DataInputStream): T
  }

  trait UnionStream[T] extends Stream[T] {
    /** True only if this stream has an encoding for `t` */
    def canEncode(t: T): Boolean
  }

  object Primitive {

    implicit object IntStream extends Stream[Int] {
      def encode(out: DataOutputStream, t: Int) = out.writeInt(t)
      def decode(in: DataInputStream): Int = in.readInt()
    }

    implicit object LongStream extends Stream[Long] {
      def encode(out: DataOutputStream, t: Long) = out.writeLong(t)
      def decode(in: DataInputStream): Long = in.readInt()
    }

    implicit object VarIntStream extends Stream[VarInt] {
      def encode(out: DataOutputStream, t: VarInt) = ox.net.codec.VarInt.writeVarInt(out, t)
      def decode(in: DataInputStream): VarInt = ox.net.codec.VarInt.readVarInt(in)
    }

    implicit object CharStream extends Stream[Char] {
      def encode(out: DataOutputStream, t: Char) = out.writeChar(t)
      def decode(in: DataInputStream): Char      = in.readChar()
    }

    implicit object ByteStream extends Stream[Byte] {
      def encode(out: DataOutputStream, t: Byte) = out.writeByte(t)
      def decode(in: DataInputStream): Byte = in.readByte()
    }

    implicit object ShortStream extends Stream[Short] {
      def encode(out: DataOutputStream, t: Short) = out.writeShort(t)
      def decode(in: DataInputStream): Short = in.readShort()
    }

    implicit object StringStream extends Stream[String] {
      def encode(out: DataOutputStream, t: String) = out.writeUTF(t)
      def decode(in: DataInputStream): String = in.readUTF()
    }

    implicit object DoubleStream extends Stream[Double] {
      def encode(out: DataOutputStream, t: Double) = out.writeDouble(t)
      def decode(in: DataInputStream): Double = in.readDouble()
    }

    implicit object FloatStream extends Stream[Float] {
      def encode(out: DataOutputStream, t: Float) = out.writeFloat(t)
      def decode(in: DataInputStream): Float = in.readFloat()
    }
  }

  class Sequence[T](implicit encoding: Stream[T]) extends Stream[Seq[T]]
  {
    def encode(out: DataOutputStream, t: Seq[T]): Unit = {
      writeVarInt(out, t.length)
      for {e <- t} encoding.encode(out, e)
    }

    def decode(in: DataInputStream): Seq[T] = {
      val length = readVarInt(in).toInt
      val result = Vector.fill[T](length){ encoding.decode(in) }
      result
    }
  }


  /**
    *   Multi-parameter case classes are encoded as tuples; the associated streams require appropriate injections/projections to be supplied:
    *   For example given the definition:
    *   {{{
    *     case class Record(name: String, age: Int)
    *   }}}
    *
    *   An appropriate implicit `Stream` is defined by:
    *   {{{
    *      implicit object RStream extends `2cons`[Record, String, Int](Record.apply, Record.unapply)
    *   }}}
    *
    *   A single-parameter case class dispenses with the tupling.
    *
    *   In due course the advent of coherent macros will permit some of the associated boilerplate
    *   to be removed; but I'm not holding my breath.
    *
    */

  class `1cons`[K, T](apply: T=>K, unapply: K=>Option[T])(implicit encoding: Stream[T]) extends UnionStream[K] {
    def encode(out: DataOutputStream, t: K): Unit = encoding.encode(out, unapply(t).get)
    def decode(in: DataInputStream): K = apply(encoding.decode(in))
    def canEncode(t: K): Boolean = unapply(t).nonEmpty
  }

  class `2cons`[K, T: Stream, U: Stream](apply: (T,U)=>K, unapply: K=>Option[(T,U)]) extends UnionStream[K] {
      val encoding = new `2tuple`[T,U]
      def encode(out: DataOutputStream, t: K): Unit = encoding.encode(out, unapply(t).get)
      def decode(in: DataInputStream): K = apply.tupled(encoding.decode(in)) // { val (t,u) = encoding.decode(in); apply(t,u) }
      def canEncode(t: K): Boolean = unapply(t).nonEmpty
    }

  /** Case classes are encoded as tuples; the associated streams require appropriate injections/projections to be supplied  */
  class `3cons`[K, T: Stream, U: Stream, V: Stream](apply: (T, U, V) => K, unapply: K => Option[(T, U, V)]) extends UnionStream[K] {
    val encoding = new `3tuple`[T, U, V]
    def encode(out: DataOutputStream, t: K): Unit = encoding.encode(out, unapply(t).get)
    def decode(in: DataInputStream): K = apply.tupled(encoding.decode(in))
    def canEncode(t: K): Boolean = unapply(t).nonEmpty
  }

  /** Case classes are encoded as tuples; the associated streams require appropriate injections/projections to be supplied */
  class `4cons`[K, T: Stream, U: Stream, V: Stream, W: Stream](apply: (T, U, V, W) => K, unapply: K => Option[(T, U, V, W)]) extends UnionStream[K] {
    val encoding = new `4tuple`[T, U, V, W]
    def encode(out: DataOutputStream, t: K): Unit = encoding.encode(out, unapply(t).get)
    def decode(in: DataInputStream): K = apply.tupled(encoding.decode(in))
    def canEncode(t: K): Boolean = unapply(t).nonEmpty
  }

  /** Case classes are encoded as tuples; the associated streams require appropriate injections/projections to be supplied */
  class `5cons`[K, T: Stream, U: Stream, V: Stream, W: Stream, X: Stream](apply: (T, U, V, W, X) => K, unapply: K => Option[(T, U, V, W, X)])
    extends UnionStream[K] {
    val encoding = new `5tuple`[T, U, V, W, X]
    def encode(out: DataOutputStream, t: K): Unit = encoding.encode(out, unapply(t).get)
    def decode(in: DataInputStream): K = apply.tupled(encoding.decode(in))
    def canEncode(t: K): Boolean = unapply(t).nonEmpty
  }


  // TODO: an algebra that extends partial functions to include (failing) coercions
  class `2union`[K, T, U] (toT: K=>T, fromT: T=>K)
                          (toU: K=>U, fromU: U=>K)
                          (implicit tenc: Stream[T], uenc: Stream[U]) extends Stream[K] {
    def encode(out: DataOutputStream, k: K): Unit = {
          try (toT.andThen { t => out.writeByte(0); tenc.encode(out, t) }) (k)
          catch {
            case exn: ClassCastException =>
              (toU.andThen { u => out.writeByte(1); uenc.encode(out, u) }) (k)
          }
    }
    def decode(in: DataInputStream): K = {
      in.readByte() match {
        case 0 => fromT(tenc.decode(in))
        case 1 => fromU(uenc.decode(in))
      }
    }
  }

  class `3union`[K, T, U, V](toT: K=>T, fromT: T => K)
                            (toU: K=>U, fromU: U => K)
                            (toV: K=>V, fromV: V => K)
                            (implicit tenc: Stream[T], uenc: Stream[U], venc: Stream[V]) extends Stream[K] {

    def encode(out: DataOutputStream, k: K): Unit = {
      try (toT.andThen { t => out.writeByte(0); tenc.encode(out, t) })(k)
      catch {
        case exn: ClassCastException =>
          try (toU.andThen { u => out.writeByte(1); uenc.encode(out, u) })(k)
          catch {
            case exn: ClassCastException =>
              (toV.andThen { v => out.writeByte(2); venc.encode(out, v) })(k)
          }
      }
    }

    def decode(in: DataInputStream): K = {
      in.readByte() match {
        case 0 => fromT(tenc.decode(in))
        case 1 => fromU(uenc.decode(in))
        case 2 => fromV(venc.decode(in))
      }
    }
  }

  class `2tuple`[T, U](implicit enc1: Stream[T], enc2: Stream[U]) extends Stream[(T, U)] {
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

  class `3tuple`[T, U, V](implicit enc1: Stream[T], enc2: Stream[U], enc3: Stream[V]) extends Stream[(T, U, V)] {
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

  class `4tuple`[T, U, V, W](implicit enc1: Stream[T], enc2: Stream[U], enc3: Stream[V], enc4: Stream[W])
    extends Stream[(T, U, V, W)] {
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

  class `5tuple`[T, U, V, W, X] (implicit enc1: Stream[T], enc2: Stream[U], enc3: Stream[V], enc4: Stream[W], enc5: Stream[X])
    extends Stream[(T, U, V, W, X)] {
    def encode(out: DataOutputStream, v: (T, U, V, W, X)): Unit = {
      enc1.encode(out, v._1)
      enc2.encode(out, v._2)
      enc3.encode(out, v._3)
      enc4.encode(out, v._4)
      enc5.encode(out, v._5)
    }

    def decode(in: DataInputStream): (T, U, V, W, X) = {
      val t = enc1.decode(in)
      val u = enc2.decode(in)
      val v = enc3.decode(in)
      val w = enc4.decode(in)
      val x = enc5.decode(in)
      (t, u, v, w, x)
    }
  }
}

object StreamEncodingInferenceTests {

  case class Record(name: String, value: Int)
  import ox.net.codec.DataStreamEncoding.Primitive._
  import ox.net.codec.DataStreamEncoding._
  implicit object Tuple2SI          extends `2tuple`[String, Int] with Stream[(String,Int)]
  implicit object RecordStream      extends `2cons`[Record, String, Int](Record.apply, Record.unapply) with Stream[Record]
  implicit object IntSequence       extends Sequence[Int]
  implicit object StringIntSequence extends Sequence[(String,Int)]
  implicit object StringSequence    extends Sequence[String]
  implicit object RecSequence       extends Sequence[Record]


  trait K
  case class B1(n:Int, s: String) extends K
  case class B2(n:Int, s: String) extends K

  implicit object BB1 extends `2cons`[B1,Int, String](B1.apply, B1.unapply)
  implicit object BB2 extends `2cons`[B2,Int, String](B2.apply, B2.unapply)
  implicit object KK extends  `2union`[K, B1, B2](_.asInstanceOf[B1], _.asInstanceOf[K])(_.asInstanceOf[B2], _.asInstanceOf[K])
  
}

