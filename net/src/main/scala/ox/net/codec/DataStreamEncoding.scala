package ox.net.codec

import ox.net.codec.VarInt._


/**
  *  Machinery to help roll-your-own stream encodings for use on
  *  cross-network transport or for "pickling" values for
  *  persistent storage.
  *
  *  Here, by convention, stream encodings are given names that end
  *  in `*`. This convention is helpful when constructing implicit objects
  *  that define encodings for composite classes.
  *
  *  The definitions here exemplify how to make a systematic and straightforward
  *  encoding that doesn't rely too much on the typeclass machinery.
  *
  *  === Warning/Advertisement:
  *
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

  class `Seq*`[T](implicit encoding: Stream[T]) extends Stream[Seq[T]]
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
    *
    *   A case class object of the form:
    *
    *   {{{
    *      case class Klass(v1: T1, ... vN: TN)
    *   }}}
    *
    *   is encoded as the `Ntuple` of its arguments. In order to
    *   acquire these arguments `Klass.unapply` must be specified, and in order to
    *   reconstruct the object `Klass.apply` must be specified.
    *
    *   Thus an implicit encoder for the above class is brought into scope by
    *   {{{
    *     implicit object `Klass*` extends `classN*`[Klass,T1, ...TN](Klass.apply, Klass.unapply)
    *   }}}
    *
    *   In due course the advent of coherent macros will permit some of the associated boilerplate
    *   to be removed; but I'm not holding my breath.
    *
    *   For the moment `N` may be between 1 and 5. Case `1` doesn't need the tupling of constructor
    *   arguments.
    *
    *   === Observation
    *
    *   Of course *any* class whose individuals objects are representable as such `N`tuples can be wire-encoded this way.
    */



  class `1case*`[K, T](apply: T=>K, unapply: K=>Option[T])(implicit encoding: Stream[T]) extends Stream[K] {
    def encode(out: DataOutputStream, t: K): Unit = encoding.encode(out, unapply(t).get)
    def decode(in: DataInputStream): K = apply(encoding.decode(in))
  }

  class `2case*`[K, T: Stream, U: Stream](apply: (T,U)=>K, unapply: K=>Option[(T,U)]) extends Stream[K] {
      val encoding = new `2tuple*`[T,U]
      def encode(out: DataOutputStream, t: K): Unit = encoding.encode(out, unapply(t).get)
      def decode(in: DataInputStream): K = apply.tupled(encoding.decode(in))
    }

  /** Case classes are encoded as tuples; the associated streams require appropriate injections/projections to be supplied  */
  class `3case*`[K, T: Stream, U: Stream, V: Stream](apply: (T, U, V) => K, unapply: K => Option[(T, U, V)]) extends Stream[K] {
    val encoding = new `3tuple*`[T, U, V]
    def encode(out: DataOutputStream, t: K): Unit = encoding.encode(out, unapply(t).get)
    def decode(in: DataInputStream): K = apply.tupled(encoding.decode(in))
  }

  /** Case classes are encoded as tuples; the associated streams require appropriate injections/projections to be supplied */
  class `4case*`[K, T: Stream, U: Stream, V: Stream, W: Stream](apply: (T, U, V, W) => K, unapply: K => Option[(T, U, V, W)]) extends Stream[K] {
    val encoding = new `4tuple*`[T, U, V, W]
    def encode(out: DataOutputStream, t: K): Unit = encoding.encode(out, unapply(t).get)
    def decode(in: DataInputStream): K = apply.tupled(encoding.decode(in))
  }

  /** Case classes are encoded as tuples; the associated streams require appropriate injections/projections to be supplied */
  class `5case*`[K, T: Stream, U: Stream, V: Stream, W: Stream, X: Stream](apply: (T, U, V, W, X) => K, unapply: K => Option[(T, U, V, W, X)])
    extends Stream[K] {
    val encoding = new `5tuple*`[T, U, V, W, X]
    def encode(out: DataOutputStream, t: K): Unit = encoding.encode(out, unapply(t).get)
    def decode(in: DataInputStream): K = apply.tupled(encoding.decode(in))
  }

  /**
    *   A case object is encoded as a `0case*`, and is wire-encoded as a single zero byte. It requires no
    *   injection/projection.
    */
  class `0case*`[K](it: K) extends Stream[K] {
    def encode(out: DataOutputStream, t: K): Unit = out.writeByte(0)

    def decode(in: DataInputStream): K = {
      assert(in.readByte()==0, "Malformed case object encoding")
      it
    }
  }

  /**
    * Enumerations, and, indeed, any types whose individuals are
    * representable as integers are wire-encoded as the integer
    * that is their representation. The injection (decoding from `Int` to
    * the type, and projection (encoding from the type to `Int`) must be given
    * explicitly when constructing a stream encoding for such types.
    *
    * For example, an implicit stream-encoding for the enumeration class defined (scala 2.12) by
    *
    * {{{
    * object E extends Enumeration {
    *    type Type = Value
    *    val v1, v2, ... = Value
    * }
    * }}}
    *
    * is constructed by
    *
    * {{{
    *   implicit object `E*` extends `Enum*`[E.Type](E, _.id)
    * }}}
    *
    */
  class `Enum*`[E](decode: Int => E, code: E => Int) extends Stream[E] {
    def encode(out: DataOutputStream, k: E) = out.writeInt(code(k))

    def decode(in: DataInputStream): E = decode(in.readInt())
  }


  /**
    *   Implements a small algebra of coercions intended to make the union-type encodings
    *   more scrutable.
    */
  private implicit class Coercion[K,TT](val coerce: K => TT) extends AnyVal {
    def apply(k: K): TT = { coerce(k) }
    def followedBy(g: TT=>Unit): K=>Unit = { k => g(coerce(k)) }
    def orElse[UU](that: Coercion[K,UU]): K=>Unit = {
      case k => try coerce(k) catch { case _: ClassCastException => that(k) }
    }
  }

  /**
    * Encoder for a union type `K` where `T` and U` both extend `K`.
    * The coercions `toT` and `toU` are intended to cast instances `k` of `K` into
    * a `T`, (or a `U`), and to fail with the exception `ClassCastException` if
    * the given `k` cannot be so cast.
    *
    * === Remark
    *
    * Usually, though not invariably, the coercions will be given as
    * `_.asInstanceOf[T]`, and `_.asInstanceOf[U]`. I think the compiler itself
    * ought to be able to infer these just from the types `K`, `T`, `U` but
    * my experiment (see below) fails at runtime.
    *
    * class `2union**`[K: ClassTag, T : ClassTag, U : ClassTag]
    *                 (implicit tenc: Stream[T], uenc: Stream[U])
    *                 extends `2union*`[K,T,U] (_.asInstanceOf[T], _.asInstanceOf[U])
    *
    */
  class `2union*`[K, T, U](toT: K=>T, toU: K=>U)
                          (implicit tenc: Stream[T], uenc: Stream[U]) extends Stream[K] {

    def encode(out: DataOutputStream, k: K): Unit = {
      val enc =
          (toT.followedBy { x => out.writeByte(0); tenc.encode(out, x) }) orElse
          (toU.followedBy { x => out.writeByte(1); uenc.encode(out, x) })
      enc(k)
    }

    def decode(in: DataInputStream): K = {
      in.readByte() match {
        case 0 => tenc.decode(in).asInstanceOf[K]
        case 1 => uenc.decode(in).asInstanceOf[K]
      }
    }
  }


  /** @see `2union*` */
  class `3union*`[K, T, U, V](toT: K=>T, toU: K=>U)(toV: K=>V)
                             (implicit tenc: Stream[T], uenc: Stream[U], venc: Stream[V]) extends Stream[K] {

    def encode(out: DataOutputStream, k: K): Unit = {
      val enc =
          (toT.followedBy { x => out.writeByte(0); tenc.encode(out, x) }) orElse
          (toU.followedBy { x => out.writeByte(1); uenc.encode(out, x) }) orElse
          (toV.followedBy { x => out.writeByte(2); venc.encode(out, x) })
      enc(k)
    }

    def decode(in: DataInputStream): K = {
      in.readByte() match {
        case 0 => tenc.decode(in).asInstanceOf[K]
        case 1 => uenc.decode(in).asInstanceOf[K]
        case 2 => venc.decode(in).asInstanceOf[K]
      }
    }
  }

  /** @see `2union*` */
  class `4union*`[K, T, U, V, W](toT: K => T, toU: K => U, toV: K => V, toW: K => W)
                                (implicit tenc: Stream[T], uenc: Stream[U], venc: Stream[V], wenc: Stream[W]) extends Stream[K] {

    def encode(out: DataOutputStream, k: K): Unit = {
      val enc =
          (toT.followedBy { x => out.writeByte(0); tenc.encode(out, x) }) orElse
          (toU.followedBy { x => out.writeByte(1); uenc.encode(out, x) }) orElse
          (toV.followedBy { x => out.writeByte(2); venc.encode(out, x) }) orElse
          (toW.followedBy { x => out.writeByte(3); wenc.encode(out, x) })
      enc(k)
    }

    def decode(in: DataInputStream): K = {
      in.readByte() match {
        case 0 => tenc.decode(in).asInstanceOf[K]
        case 1 => uenc.decode(in).asInstanceOf[K]
        case 2 => venc.decode(in).asInstanceOf[K]
        case 3 => wenc.decode(in).asInstanceOf[K]
      }
    }
  }

  /** @see `2union*` */
  class `5union*`[K, T, U, V, W, X](toT: K => T, toU: K => U, toV: K => V, toW: K => W, toX: K=>X)
                                   (implicit tenc: Stream[T], uenc: Stream[U], venc: Stream[V], wenc: Stream[W], xenc: Stream[X]) extends Stream[K] {

    def encode(out: DataOutputStream, k: K): Unit = {
      val enc =
          (toT.followedBy { x => out.writeByte(0); tenc.encode(out, x) }) orElse
          (toU.followedBy { x => out.writeByte(1); uenc.encode(out, x) }) orElse
          (toV.followedBy { x => out.writeByte(2); venc.encode(out, x) }) orElse
          (toW.followedBy { x => out.writeByte(3); wenc.encode(out, x) }) orElse
          (toX.followedBy { x => out.writeByte(4); xenc.encode(out, x) })
      enc(k)
    }

    def decode(in: DataInputStream): K = {
      in.readByte() match {
        case 0 => tenc.decode(in).asInstanceOf[K]
        case 1 => uenc.decode(in).asInstanceOf[K]
        case 2 => venc.decode(in).asInstanceOf[K]
        case 3 => wenc.decode(in).asInstanceOf[K]
        case 4 => xenc.decode(in).asInstanceOf[K]
      }
    }
  }

  /** Encode a 2-tuple as the catenation of its component encodings */
  class `2tuple*`[T, U](implicit enc1: Stream[T], enc2: Stream[U]) extends Stream[(T, U)] {
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

  /** Encode a 3-tuple as the catenation of its component encodings */
  class `3tuple*`[T, U, V](implicit enc1: Stream[T], enc2: Stream[U], enc3: Stream[V]) extends Stream[(T, U, V)] {
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

  /** Encode a 4-tuple as the catenation of its component encodings */
  class `4tuple*`[T, U, V, W](implicit enc1: Stream[T], enc2: Stream[U], enc3: Stream[V], enc4: Stream[W])
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

  /** Encode a 5-tuple as the catenation of its component encodings */
  class `5tuple*`[T, U, V, W, X] (implicit enc1: Stream[T], enc2: Stream[U], enc3: Stream[V], enc4: Stream[W], enc5: Stream[X])
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
  implicit object Tuple2SI          extends `2tuple*`[String, Int] with Stream[(String,Int)]
  implicit object RecordStream      extends `2case*`[Record, String, Int](Record.apply, Record.unapply) with Stream[Record]
  implicit object IntSequence       extends `Seq*`[Int]
  implicit object StringIntSequence extends `Seq*`[(String,Int)]
  implicit object StringSequence    extends `Seq*`[String]
  implicit object RecSeq            extends `Seq*`[Record]


  trait K
  case class B1(n:Int, s: String) extends K
  case class B2(n:Int, s: String) extends K

  object E extends Enumeration {
    val v1, v2, v3 = Value
    type Type = Value
  }

  implicit object `B1*` extends `2case*`[B1,Int, String](B1.apply, B1.unapply)
  implicit object `B2*` extends `2case*`[B2,Int, String](B2.apply, B2.unapply)
  implicit object `K*` extends  `2union*`[K, B1, B2](_.asInstanceOf[B1], _.asInstanceOf[B2])
  implicit object `E*` extends  `Enum*`[E.Type](E.apply, _.id)

}

