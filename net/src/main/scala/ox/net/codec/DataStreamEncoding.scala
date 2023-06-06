package ox.net.codec

import ox.net.codec.VarInt._

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.math.{BigDecimal => BigJavaDecimal, BigInteger => BigJavaInteger}
import scala.reflect.ClassTag


/**
  *  Machinery to help roll-your-own stream encodings for use on
  *  cross-network transport or for "pickling" values for
  *  persistent storage.
  *
  *  Here, by convention, stream encodings for non-simple types are given names that end
  *  in `*`. This convention is helpful when constructing implicit objects
  *  that define encodings for composite classes.
  *
  *  The definitions here exemplify how to make a systematic and straightforward
  *  stream encoding that doesn't rely too much on the typeclass machinery.
  *
  * @see StreamEncodingShortTests for several examples of the use of these constructions.
  *
  *  === Warning/Advertisement:
  *
  *  The `msgpack` machinery delivers more compact streams -- albeit at some
  *  cost in complexity, and in computing the compressed representation. But its killer
  *  advantage is interoperability because there are many language bindings for `msgpack`.
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

    implicit object `Int*` extends Stream[Int] {
      def encode(out: DataOutputStream, t: Int) = out.writeInt(t)
      def decode(in: DataInputStream): Int = in.readInt()
    }

    implicit object `Long*` extends Stream[Long] {
      def encode(out: DataOutputStream, t: Long) = out.writeLong(t)
      def decode(in: DataInputStream): Long = in.readInt()
    }

    implicit object `VarInt*` extends Stream[VarInt] {
      def encode(out: DataOutputStream, t: VarInt) = ox.net.codec.VarInt.writeVarInt(out, t)
      def decode(in: DataInputStream): VarInt = ox.net.codec.VarInt.readVarInt(in)
    }

    implicit object `Char*` extends Stream[Char] {
      def encode(out: DataOutputStream, t: Char) = out.writeChar(t)
      def decode(in: DataInputStream): Char      = in.readChar()
    }

    implicit object `Byte*` extends Stream[Byte] {
      def encode(out: DataOutputStream, t: Byte) = out.writeByte(t)
      def decode(in: DataInputStream): Byte = in.readByte()
    }

    implicit object `Short*` extends Stream[Short] {
      def encode(out: DataOutputStream, t: Short) = out.writeShort(t)
      def decode(in: DataInputStream): Short = in.readShort()
    }

    implicit object `String*` extends Stream[String] {
      def encode(out: DataOutputStream, t: String) = out.writeUTF(t)
      def decode(in: DataInputStream): String = in.readUTF()
    }

    implicit object `Double*` extends Stream[Double] {
      def encode(out: DataOutputStream, t: Double) = out.writeDouble(t)
      def decode(in: DataInputStream): Double = in.readDouble()
    }

    implicit object `Float*` extends Stream[Float] {
      def encode(out: DataOutputStream, t: Float) = out.writeFloat(t)
      def decode(in: DataInputStream): Float = in.readFloat()
    }

    implicit object `BigJavaDecimal*` extends Stream[BigJavaDecimal] {
      def encode(out: DataOutputStream, t: BigJavaDecimal) = {
        out.writeInt(t.scale)
        val rep = t.unscaledValue.toByteArray
        out.writeInt(rep.length)
        for { b <- rep } out.writeByte(b)
      }

      def decode(in: DataInputStream): BigJavaDecimal = {
          val scale = in.readInt()
          val length = in.readInt()
          val rep = Array.ofDim[Byte](length)
          for { i<-0 until length } rep(i) = in.readByte().toByte
          new BigJavaDecimal(new BigJavaInteger(rep), scale)
      }
    }

    implicit object `BigJavaInteger*` extends Stream[BigJavaInteger] {
      def encode(out: DataOutputStream, t: BigJavaInteger) = {
        val rep = t.toByteArray
        out.writeInt(rep.length)
        for {b <- rep} out.writeByte(b)
      }

      def decode(in: DataInputStream): BigJavaInteger = {
        val length = in.readInt()
        val rep: Array[Byte] = Array.ofDim[Byte](length)
        for {i <- 0 until length} rep(i) = in.readByte().toByte
        new BigJavaInteger(rep)
      }
    }

    implicit object `BigInt*` extends Stream[BigInt] {
      def encode(out: DataOutputStream, t: BigInt) = {
        `BigJavaInteger*`.encode(out, t.underlying())
      }

      def decode(in: DataInputStream): BigInt = {
          BigInt(`BigJavaInteger*`.decode(in))
      }
    }


    implicit object `BigDecimal*` extends Stream[BigDecimal] {
      def encode(out: DataOutputStream, t: BigDecimal) = {
        `BigJavaDecimal*`.encode(out, t.underlying())
      }

      def decode(in: DataInputStream): BigDecimal = {
        `BigJavaDecimal*`.decode(in)
      }
    }

  class `Seq*`[T](implicit enc: Stream[T]) extends Stream[Seq[T]]
  {
    def encode(out: DataOutputStream, t: Seq[T]): Unit = {
      writeVarInt(out, t.length)
      for {e <- t} enc.encode(out, e)
    }

    def decode(in: DataInputStream): Seq[T] = {
      val length = readVarInt(in).toInt
      val result = Vector.fill[T](length){ enc.decode(in) }
      result
    }
  }

  class `Option*`[T](implicit enc: Stream[T]) extends Stream[Option[T]] {
    def encode(out: DataOutputStream, t: Option[T]): Unit = {
      t match {
        case None    => out.writeBoolean(false)
        case Some(t) =>
          out.writeBoolean(true)
          enc.encode(out, t)
      }
    }

    def decode(in: DataInputStream): Option[T] = {
      in.readBoolean() match {
        case false => None
        case true  =>  Some(enc.decode(in))
      }
    }
  }

  class `List*`[T](implicit enc: Stream[T]) extends Stream[List[T]] {
    def encode(out: DataOutputStream, t: List[T]): Unit = {
      writeVarInt(out, t.length)
      for {e <- t} enc.encode(out, e)
    }

    def decode(in: DataInputStream): List[T] = {
      val length = readVarInt(in).toInt
      val result = Vector.fill[T](length) {
        enc.decode(in)
      }
      result.toList
    }
  }

  /** Array evidence needs the type to have a class tag */
  class `Array*`[T: ClassTag](implicit enc: Stream[T]) extends Stream[scala.Array[T]] {
    def encode(out: DataOutputStream, t: scala.Array[T]): Unit = {
      writeVarInt(out, t.length)
      for {e <- t} enc.encode(out, e)
    }

    def decode(in: DataInputStream): scala.Array[T] = {
      val length = readVarInt(in).toInt
      val result = scala.Array.ofDim[T](length)
          for  { i<-0 until length } result(i) = enc.decode(in)
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
    *
    */



  class `1-Case*`[K, T](apply: T=>K, unapply: K=>Option[T])(implicit enc: Stream[T]) extends Stream[K] {
    def encode(out: DataOutputStream, t: K): Unit = enc.encode(out, unapply(t).get)
    def decode(in: DataInputStream): K = apply(enc.decode(in))
  }

  class `2-Case*`[K, T: Stream, U: Stream](apply: (T,U)=>K, unapply: K=>Option[(T,U)]) extends Stream[K] {
      val enc = new `2-Tuple*`[T,U]
      def encode(out: DataOutputStream, t: K): Unit = enc.encode(out, unapply(t).get)
      def decode(in: DataInputStream): K = apply.tupled(enc.decode(in))
    }

  /** Case classes are encoded as tuples; the associated streams require appropriate injections/projections to be supplied  */
  class `3-Case*`[K, T: Stream, U: Stream, V: Stream](apply: (T, U, V) => K, unapply: K => Option[(T, U, V)]) extends Stream[K] {
    val enc = new `3tuple*`[T, U, V]
    def encode(out: DataOutputStream, t: K): Unit = enc.encode(out, unapply(t).get)
    def decode(in: DataInputStream): K = apply.tupled(enc.decode(in))
  }

  /** Case classes are encoded as tuples; the associated streams require appropriate injections/projections to be supplied */
  class `4-Case*`[K, T: Stream, U: Stream, V: Stream, W: Stream](apply: (T, U, V, W) => K, unapply: K => Option[(T, U, V, W)]) extends Stream[K] {
    val enc = new `4-Tuple*`[T, U, V, W]
    def encode(out: DataOutputStream, t: K): Unit = enc.encode(out, unapply(t).get)
    def decode(in: DataInputStream): K = apply.tupled(enc.decode(in))
  }

  /** Case classes are encoded as tuples; the associated streams require appropriate injections/projections to be supplied */
  class `5-Case*`[K, T: Stream, U: Stream, V: Stream, W: Stream, X: Stream](apply: (T, U, V, W, X) => K, unapply: K => Option[(T, U, V, W, X)])
    extends Stream[K] {
    val enc = new `5-Tuple*`[T, U, V, W, X]
    def encode(out: DataOutputStream, t: K): Unit = enc.encode(out, unapply(t).get)
    def decode(in: DataInputStream): K = apply.tupled(enc.decode(in))
  }

  /**
    *   A case object is wire-encoded as a single zero byte. It requires no
    *   injection/projection.
    */
  class `Case*`[K](it: K) extends Stream[K] {
    def encode(out: DataOutputStream, t: K): Unit = out.writeByte(0)

    def decode(in: DataInputStream): K = {
      assert(in.readByte()==0, "Malformed case object enc")
      it
    }
  }

  /**
    * Enumerations, indeed, any types whose individuals are
    * representable as integers, are wire-encoded as the integer
    * that is their representation. The projection (decoding from `Int` to
    * the type, and injection (enc from the type to `Int`) must be given
    * explicitly when constructing a stream enc for such types (and must be
    * mutual inverses).
    *
    * For example, an implicit stream-enc for the enumeration class defined (scala 2.12) by
    *
    * {{{
    *         object E extends Enumeration { val v1, v2, ... = Value }
    * }}}
    * is constructed by
    * {{{
    *         implicit object `E*` extends `Enum*`[E.Value](E, _.id)
    * }}}
    *
    * It is quite normal to name the type (`Value`) constructed by an `Enumeration` object, so the following
    * will also suffice:
    * {{{
    *         object E extends Enumeration { val v1, v2, ... = Value }
    *         type E = E.Value
  *           implicit object `E*` extends `Enum*`[E](E, _.id)
    * }}}
    *
    *
    */
  class `Enum*`[E](decoding: Int => E, encoding: E => Int) extends Stream[E] {
    def encode(out: DataOutputStream, k: E) = out.writeInt(encoding(k))
    def decode(in: DataInputStream): E = decoding(in.readInt())
  }

  /**
    *  Values of type `T` may be be wire-encoded as values of type
    *  `CODE` -- which must itself have a wire enc. Simply
    *  supply an injection `enc:T=>CODE` and the corresponding inverse
    *  `decoding:CODE=>T`.
    *
    * === Remark
    *
    * In fact the following are operationally equivalent
    * {{{`Enum*`[E] (decoding: Int => E, enc: E => Int)}}}
    * and
    * {{{`Encode*`[E,Int](decoding, enc)}}}
    */
  class `Encode*`[T,CODE](decoding: CODE=>T, encoding: T=>CODE)(implicit CODEEnc: Stream[CODE]) extends Stream[T] {
    def encode(out: DataOutputStream, k: T) = CODEEnc.encode(out, encoding(k))
    def decode(in: DataInputStream): T = decoding(CODEEnc.decode(in))
  }

  /** Sets encoded as sequences */
  class `Set*`[T](implicit enc: Stream[Seq[T]]) extends `Encode*`[Set[T], Seq[T]](_.toSet, _.toSeq)

  /** Maps encoded as 2-tuple sequences */
  class `Map*`[K,V](implicit enc: Stream[Seq[(K,V)]]) extends `Encode*`[Map[K,V], Seq[(K,V)]](_.toMap, _.toSeq)

    // Notice that class `Enum*`[E] (decode: Int => E, code: E => Int) == (E `EncodedAs*` Int)(decode, code)


  /**
    *   Implements a small algebra of coercions intended to make the union-type encodings
    *   more scrutable.
    */
  private implicit class Coercion[K,TT](val coerce: K => TT) extends AnyVal {
    def apply(k: K): TT = { coerce(k) }
    def followedBy(g: TT=>Unit): K=>Unit = { k => g(coerce(k)) }
    def orElse[UU](that: Coercion[K,UU]): K=>Unit = {
      case k => try coerce(k) catch {
        case _: ClassCastException => that(k)
        case _: MatchError         => that(k) // caused by an identity coercion on a case object
      }
    }
  }

  /** A "case object" cast for use in unions */
  def asObject[K,T](b:T): (K=>T) = { case k:K @unchecked if k==b => b}

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
    * my experiment (see below) fails at runtime, as do similar ones that also
    * compile without complaint.
    *
    * class `2-Union**`[K: ClassTag, T : ClassTag, U : ClassTag]
    *                  (implicit tenc: Stream[T], uenc: Stream[U])
    *                  extends `2union*`[K,T,U] (_.asInstanceOf[T], _.asInstanceOf[U])
    *
    */
  class `2-Union*`[K, T, U](toT: K=>T, toU: K=>U)
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


  /** @see `2-Union*` */
  class `3-Union*`[K, T, U, V](toT: K=>T, toU: K=>U, toV: K=>V)
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

  /** @see `2-Union*` */
  class `4-Union*`[K, T, U, V, W](toT: K => T, toU: K => U, toV: K => V, toW: K => W)
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

  /** @see `2-Union*` */
  class `5-Union*`[K, T, U, V, W, X](toT: K => T, toU: K => U, toV: K => V, toW: K => W, toX: K=>X)
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
  class `2-Tuple*`[T, U](implicit enc1: Stream[T], enc2: Stream[U]) extends Stream[(T, U)] {
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
  class `4-Tuple*`[T, U, V, W](implicit enc1: Stream[T], enc2: Stream[U], enc3: Stream[V], enc4: Stream[W])
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

  /** Encode a 5-tuple as the catenation of its component encodings
  class `5-Tuple*`[T, U, V, W, X](implicit enc1: Stream[T], enc2: Stream[U], enc3: Stream[V], enc4: Stream[W], enc5: Stream[X])
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
  */

  class `6-Tuple*`[T1: Stream, T2: Stream, T3: Stream, T4: Stream, T5: Stream, T6: Stream]
    extends Stream[(T1, T2, T3, T4, T5, T6)] {
    val enc1 = implicitly[Stream[T1]]
    val enc2 = implicitly[Stream[T2]]
    val enc3 = implicitly[Stream[T3]]
    val enc4 = implicitly[Stream[T4]]
    val enc5 = implicitly[Stream[T5]]
    val enc6 = implicitly[Stream[T6]]

    def encode(out: DataOutputStream, v: (T1, T2, T3, T4, T5, T6)): Unit = {
      enc1.encode(out, v._1)
      enc2.encode(out, v._2)
      enc3.encode(out, v._3)
      enc4.encode(out, v._4)
      enc5.encode(out, v._5)
      enc6.encode(out, v._6)
    }

    def decode(in: DataInputStream): (T1, T2, T3, T4, T5, T6) = {
      val t1 = enc1.decode(in)
      val t2 = enc2.decode(in)
      val t3 = enc3.decode(in)
      val t4 = enc4.decode(in)
      val t5 = enc5.decode(in)
      val t6 = enc6.decode(in)
      (t1, t2, t3, t4, t5, t6)
    }
  }

  class `5-Tuple*`[T1: Stream, T2: Stream, T3: Stream, T4: Stream, T5: Stream]
    extends Stream[(T1, T2, T3, T4, T5)] {
    val enc1 = implicitly[Stream[T1]]
    val enc2 = implicitly[Stream[T2]]
    val enc3 = implicitly[Stream[T3]]
    val enc4 = implicitly[Stream[T4]]
    val enc5 = implicitly[Stream[T5]]

    def encode(out: DataOutputStream, v: (T1, T2, T3, T4, T5)): Unit = {
      enc1.encode(out, v._1)
      enc2.encode(out, v._2)
      enc3.encode(out, v._3)
      enc4.encode(out, v._4)
      enc5.encode(out, v._5)
    }

    def decode(in: DataInputStream): (T1, T2, T3, T4, T5) = {
      val t1 = enc1.decode(in)
      val t2 = enc2.decode(in)
      val t3 = enc3.decode(in)
      val t4 = enc4.decode(in)
      val t5 = enc5.decode(in)
      (t1, t2, t3, t4, t5)
    }
  }

  /**
    * Use the implicit stream encoding/decoding for `T` to round-trip `value: T`,
    * and check that the decoded result is `value`.
    */
  def encodingTest[T](value: T)(implicit enc: Stream[T]): Unit = {
    val bytes = new ByteArrayOutputStream()
    val out = new DataOutputStream(bytes)
    enc.encode(out, value)
    val streamed = bytes.toByteArray
    val in = new DataInputStream(new ByteArrayInputStream(streamed))
    val decoded = enc.decode(in)
    (value, decoded) match {
      case (v: Array[Any], d: Array[Any]) if v.toList == d.toList => println(s"OK ${v.toList.mkString("Array(", ", ", ")")} as an equal array. (${streamed.length} bytes)")
      case (_, _) if value==decoded => println(s"OK ${value.toString.take(70)}... (${streamed.length} bytes)")
      case (_, _) => println(s"**** Roundtrip failure\n**** Encoded: $value\n**** Decoded: $decoded")
    }
  }
}




