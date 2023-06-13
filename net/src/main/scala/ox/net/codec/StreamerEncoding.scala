package ox.net.codec

import ox.net.codec.VarInt._

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream, ObjectStreamConstants, StreamCorruptedException}
import java.math.{BigDecimal => BigJavaDecimal, BigInteger => BigJavaInteger}
import scala.reflect.ClassTag

/**
  *  A stream(er) encoding is an injective mapping between values of type `T` and the sequence of
  *  bytes that is used to represent values of that type.
  *
  *  Here we provide straightforward stream encodings for use on cross-network transport or
  *  for "pickling" values for persistent storage.
  *
  *  The resulting byte-sequences are not "self-describing": to decode a byte-sequence
  *  encoded by  `enc.toStream...` it is necessary to use the corresponding `enc.fromStream`.
  *
  *  By convention the predefined stream encodings (and encoding combinators) have names that end
  *  in `*`, and we advise the use of the same convention when
  *  constructing objects that define encodings for composite classes. For example
  *
  *  {{{
  *    implicit encoding `Seq[Int]*` extends `Seq*`[Int]
  *    implicit encoding `Opt[Seq[Int]]*` extends `Opt*`[Seq[Int]]
  *    implicit encoding `(Int, Seq[Int])*` extends
  *             `2-Tuple`[(Int, Seq[Int])]
  *  }}}
  *
  *  The definitions here exemplify a complete, and systematic collection of
  *  stream encodings that is not overreliant Scala's machinery of implicits, and is independent of
  *  any macro definitions. They rely for their soundness on the mutual invertibility of the java
  *  `Data(Output/Input)Streamer` representations of primitives, as loosely described
  *  in the java documentation for such streams.
  *
  *  See `ox.net.codec.StreamerEncodingTests` for several examples of the use of these
  *  constructions, and `ox.net.ManualTests` for an example of how an encoding can
  *  be used in an `ox.net`-based concurrent program.
  *
  *  == Alternatives:
  *
  *  The `msgpack` machinery provided here delivers more compact encodings -- albeit at some
  *  cost in complexity, and in computing the compressed representation. But its real
  *  advantage is its interoperability and explicit bit-level specification -- which
  *  means there are already many language bindings for `msgpack`.
  *
  *  The `borer` implementation of the `CBOR` standard (`https://www.rfc-editor.org/rfc/rfc8949.html`)
  *  was designed and built in the same spirit, and seems more modern, and very versatile. Importantly,
  *  it provides more-or-less automatic ways of deriving the code for stream-encodings.
  *
  * @see https://sirthias.github.io/borer/index.html
  * @see ox.net.VelviaChannelFactory, org.velvia.MessagePack
  */
object StreamerEncoding {

  import java.io.{DataInputStream, DataOutputStream}

  /**
    *  A `Streamer[T]` effectively defines the injective correspondence between
    *  data of type `T`, and its encoding as a sequence of bytes.
    *
    *  === Specification
    *
    *  Call this injection `byteRep`, and recall that (the state of) a `DataOutputStream` represents
    *  a sequence of bytes, as does the state of a `DataInputStream`.
    *
    *  Suppose `out` currently represents the byte-sequence `outSeq`, then
    *  after `toStream(out, t)`, `out` represents `outSeq++byteRep(t)`.
    *
    *  Suppose `t:T` is such that `in` represents the byte-sequence `byteRep(t)++inSeq`, then
    *  the result of `fromStream(in)` is `t`, and `in` subsequently represents
    *  the byte-sequence `inSeq`.
    *
    *  If there is no `t` such that `byteRep(t)` prefixes the byte-sequence currently
    *  represented by `in`, then `fromStream(in)` can do anything at all.
    *
    *  The injectivity of `byteRep` is essential to the "round-trip integrity" of a streamer,
    *  without which the very idea of a stream encoding is useless.
    */
    trait Streamer[T] {
      /**
        * Append the encoding of `datum` to the `out` stream
        */
      def toStream(out: DataOutputStream, data: T): Unit

      /**
        * Remove the encoding of a datum of type `T from thefront of the `in` stream,
        * and return the datum.
        */
      def fromStream(in: DataInputStream): T
    }

    implicit object `Int*` extends Streamer[Int] {
      def toStream(out: DataOutputStream, t: Int): Unit = out.writeInt(t)
      def fromStream(in: DataInputStream): Int = in.readInt()
    }

    implicit object `Long*` extends Streamer[Long] {
      def toStream(out: DataOutputStream, t: Long): Unit  = out.writeLong(t)
      def fromStream(in: DataInputStream): Long = in.readInt()
    }

    implicit object `VarInt*` extends Streamer[VarInt] {
      def toStream(out: DataOutputStream, t: VarInt): Unit  = ox.net.codec.VarInt.writeVarInt(out, t)
      def fromStream(in: DataInputStream): VarInt = ox.net.codec.VarInt.readVarInt(in)
    }

    implicit object `Char*` extends Streamer[Char] {
      def toStream(out: DataOutputStream, t: Char): Unit  = out.writeChar(t)
      def fromStream(in: DataInputStream): Char      = in.readChar()
    }

    implicit object `Byte*` extends Streamer[Byte] {
      def toStream(out: DataOutputStream, t: Byte): Unit  = out.writeByte(t)
      def fromStream(in: DataInputStream): Byte = in.readByte()
    }

    implicit object `Short*` extends Streamer[Short] {
      def toStream(out: DataOutputStream, t: Short): Unit  = out.writeShort(t)
      def fromStream(in: DataInputStream): Short = in.readShort()
    }

    implicit object `String*` extends Streamer[String] {
      def toStream(out: DataOutputStream, t: String): Unit  = out.writeUTF(t)
      def fromStream(in: DataInputStream): String = in.readUTF()
    }

    implicit object `Double*` extends Streamer[Double] {
      def toStream(out: DataOutputStream, t: Double): Unit  = out.writeDouble(t)
      def fromStream(in: DataInputStream): Double = in.readDouble()
    }

    implicit object `Float*` extends Streamer[Float] {
      def toStream(out: DataOutputStream, t: Float): Unit  = out.writeFloat(t)
      def fromStream(in: DataInputStream): Float = in.readFloat()
    }

    implicit object `BigJavaDecimal*` extends Streamer[BigJavaDecimal] {
      def toStream(out: DataOutputStream, t: BigJavaDecimal): Unit  = {
        out.writeInt(t.scale)
        val rep = t.unscaledValue.toByteArray
        out.writeInt(rep.length)
        for { b <- rep } out.writeByte(b)
      }

      def fromStream(in: DataInputStream): BigJavaDecimal = {
          val scale = in.readInt()
          val length = in.readInt()
          val rep = Array.ofDim[Byte](length)
          for { i<-0 until length } rep(i) = in.readByte()
          new BigJavaDecimal(new BigJavaInteger(rep), scale)
      }
    }

    implicit object `BigJavaInteger*` extends Streamer[BigJavaInteger] {
      def toStream(out: DataOutputStream, t: BigJavaInteger): Unit  = {
        val rep = t.toByteArray
        out.writeInt(rep.length)
        for {b <- rep} out.writeByte(b)
      }

      def fromStream(in: DataInputStream): BigJavaInteger = {
        val length = in.readInt()
        val rep: Array[Byte] = Array.ofDim[Byte](length)
        for {i <- 0 until length} rep(i) = in.readByte()
        new BigJavaInteger(rep)
      }
    }

    implicit object `BigInt*` extends Streamer[BigInt] {
      def toStream(out: DataOutputStream, t: BigInt): Unit  = {
        `BigJavaInteger*`.toStream(out, t.underlying())
      }

      def fromStream(in: DataInputStream): BigInt = {
          BigInt(`BigJavaInteger*`.fromStream(in))
      }
    }

    implicit object `BigDecimal*` extends Streamer[BigDecimal] {
      def toStream(out: DataOutputStream, t: BigDecimal): Unit  = {
        `BigJavaDecimal*`.toStream(out, t.underlying())
      }

      def fromStream(in: DataInputStream): BigDecimal = {
        `BigJavaDecimal*`.fromStream(in)
      }
    }

  class `Seq*`[T : Streamer] extends Streamer[Seq[T]]  {
    private val enc = implicitly[Streamer[T]]

    def toStream(out: DataOutputStream, t: Seq[T]): Unit = {
      writeVarInt(out, t.length)
      for {e <- t} enc.toStream(out, e)
    }

    def fromStream(in: DataInputStream): Seq[T] = {
      val length = readVarInt(in).toInt
      val result = Vector.fill[T](length){ enc.fromStream(in) }
      result
    }
  }

  /**
    *   An encoding for `Seq[T]` that requires the encoding for `T` to be made explicit
    *   This is not strictly needed, but some derived encoding specifications turn into
    *   one-liners when it's used.
    */
  object `Seq*` {
    def apply[T](enc: Streamer[T]): Streamer[Seq[T]] = new Streamer[Seq[T]] {
      def toStream(out: DataOutputStream, t: Seq[T]): Unit = {
        writeVarInt(out, t.length)
        for {e <- t} enc.toStream(out, e)
      }

      def fromStream(in: DataInputStream): Seq[T] = {
        val length = readVarInt(in).toInt
        val result = Vector.fill[T](length) {
          enc.fromStream(in)
        }
        result
      }
    }
  }

  /** An encoding for `Option[T]` */
  class `Option*`[T : Streamer] extends Streamer[Option[T]] {
    val enc = implicitly[Streamer[T]]
    def toStream(out: DataOutputStream, t: Option[T]): Unit = {
      t match {
        case None    =>
          out.writeBoolean(false)
        case Some(t) =>
          out.writeBoolean(true)
          enc.toStream(out, t)
      }
    }

    def fromStream(in: DataInputStream): Option[T] = {
      if (in.readBoolean()) {
        Some(enc.fromStream(in))
      } else {
        None
      }
    }
  }

  class `Either*`[L: Streamer, R: Streamer] extends Streamer[Either[L,R]] {
    val encl = implicitly[Streamer[L]]
    val encr = implicitly[Streamer[R]]
    /**
      * Append the encoding of `datum` to the `out` stream
      */
    def toStream(out: DataOutputStream, data: Either[L, R]): Unit = {
      data match {
        case Left(d) => out.writeByte(0);  encl.toStream(out, d)
        case Right(d) => out.writeByte(1); encr.toStream(out, d)
      }
    }

    /**
      * Remove the encoding of a datum of type `T from the front of the `in` stream,
      * and return the datum.
      */
    def fromStream(in: DataInputStream): Either[L, R] = {
      in.readByte()  match {
        case 0 => Left(encl.fromStream(in))
        case 1 => Right(encr.fromStream(in))
      }
    }
  }

  /** An encoding for `List[T]` */
  class `List*`[T : Streamer] extends Streamer[List[T]] {
    val enc = implicitly[Streamer[T]]
    def toStream(out: DataOutputStream, t: List[T]): Unit = {
      writeVarInt(out, t.length)
      for {e <- t} enc.toStream(out, e)
    }

    def fromStream(in: DataInputStream): List[T] = {
      val length = readVarInt(in).toInt
      val result = Vector.fill[T](length) {
        enc.fromStream(in)
      }
      result.toList
    }
  }

  /** A streamer for `Array[T]` it needs `T` to have a class tag */
  class `Array*`[T: ClassTag](implicit enc: Streamer[T]) extends Streamer[scala.Array[T]] {
    def toStream(out: DataOutputStream, t: scala.Array[T]): Unit = {
      writeVarInt(out, t.length)
      for {e <- t} enc.toStream(out, e)
    }

    def fromStream(in: DataInputStream): scala.Array[T] = {
      val length = readVarInt(in).toInt
      val result = scala.Array.ofDim[T](length)
          for  { i<-0 until length } result(i) = enc.fromStream(in)
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
    *   Any class whose individual objects are representable as such `N`tuples can be wire-encoded this way.
    *
    */

  class `1-Case*`[K, T](apply: T=>K, unapply: K=>Option[T])(implicit enc: Streamer[T]) extends Streamer[K] {
    def toStream(out: DataOutputStream, t: K): Unit = enc.toStream(out, unapply(t).get)
    def fromStream(in: DataInputStream): K = apply(enc.fromStream(in))
  }

  /** @see `1-Case*` */
  class `2-Case*`[K, T: Streamer, U: Streamer](apply: (T,U)=>K, unapply: K=>Option[(T,U)]) extends Streamer[K] {
      private val enc = new `2-Tuple*`[T,U]
      def toStream(out: DataOutputStream, t: K): Unit = enc.toStream(out, unapply(t).get)
      def fromStream(in: DataInputStream): K = apply.tupled(enc.fromStream(in))
    }

  /** @see `1-Case*` */
  class `3-Case*`[K, T: Streamer, U: Streamer, V: Streamer](apply: (T, U, V) => K, unapply: K => Option[(T, U, V)]) extends Streamer[K] {
    private val enc = new `3-Tuple*`[T, U, V]
    def toStream(out: DataOutputStream, t: K): Unit = enc.toStream(out, unapply(t).get)
    def fromStream(in: DataInputStream): K = apply.tupled(enc.fromStream(in))
  }

  /** @see `1-Case*` */
  class `4-Case*`[K, T: Streamer, U: Streamer, V: Streamer, W: Streamer](apply: (T, U, V, W) => K, unapply: K => Option[(T, U, V, W)]) extends Streamer[K] {
    private val enc = new `4-Tuple*`[T, U, V, W]
    def toStream(out: DataOutputStream, t: K): Unit = enc.toStream(out, unapply(t).get)
    def fromStream(in: DataInputStream): K = apply.tupled(enc.fromStream(in))
  }

  /** @see `1-Case*` */
  class `5-Case*`[K, T: Streamer, U: Streamer, V: Streamer, W: Streamer, X: Streamer](apply: (T, U, V, W, X) => K, unapply: K => Option[(T, U, V, W, X)])
    extends Streamer[K] {
    private val enc = new`5-Tuple*`[T, U, V, W, X]

    def toStream(out: DataOutputStream, t: K): Unit = enc.toStream(out, unapply(t).get)

    def fromStream(in: DataInputStream): K = apply.tupled(enc.fromStream(in))
  }

  /** @see `1-Case*` */
  class `6-Case*`[K, T: Streamer, U: Streamer, V: Streamer, W: Streamer, X: Streamer, Y: Streamer](apply: (T, U, V, W, X, Y) => K,
                                                                                                   unapply: K => Option[(T, U, V, W, X, Y)])
    extends Streamer[K] {
    private val enc = new`6-Tuple*`[T, U, V, W, X, Y]
    def toStream(out: DataOutputStream, t: K): Unit = enc.toStream(out, unapply(t).get)
    def fromStream(in: DataInputStream): K = apply.tupled(enc.fromStream(in))
  }


  /** 2-Case with explicit encodings passed by name: for when one of the component types is (or references) `K` */
  class `2-Case-Rec*`[K, T, U](apply: (T, U) => K, unapply: K => Option[(T, U)])(tenc: => Streamer[T], uenc: => Streamer[U]) extends Streamer[K] {

    def toStream(out: DataOutputStream, t: K): Unit = {
      val Some((tt, uu)) = unapply(t)
      tenc.toStream(out, tt)
      uenc.toStream(out, uu)
    }

    def fromStream(in: DataInputStream): K = {
      val tt = tenc.fromStream(in)
      val uu = uenc.fromStream(in)
      apply(tt, uu)
    }
  }

  /** 3-Case with explicit encodings passed by name: for when one of the component types is (or references) `K` */
  class `3-Case-Rec*`[K, T, U, V](apply: (T, U, V) => K, unapply: K => Option[(T, U, V)])(tenc: => Streamer[T], uenc: => Streamer[U], venc: => Streamer[V]) extends Streamer[K] {

    def toStream(out: DataOutputStream, t: K): Unit = {
      val Some((tt, uu, vv)) = unapply(t)
      tenc.toStream(out, tt)
      uenc.toStream(out, uu)
      venc.toStream(out, vv)
    }

    def fromStream(in: DataInputStream): K = {
      val tt = tenc.fromStream(in)
      val uu = uenc.fromStream(in)
      val vv = venc.fromStream(in)
      apply(tt, uu, vv)
    }
  }

  /** 4-Case with explicit encodings passed by name: for when one of the component types is (or references) `K` */
  class `4-Case-Rec*`[K, T, U, V, W](apply: (T, U, V, W) => K, unapply: K => Option[(T, U, V, W)])
                                 (tenc: => Streamer[T], uenc: => Streamer[U], venc: => Streamer[V], wenc: => Streamer[W]) extends Streamer[K] {

    def toStream(out: DataOutputStream, t: K): Unit = {
      val Some((tt, uu, vv, ww)) = unapply(t)
      tenc.toStream(out, tt)
      uenc.toStream(out, uu)
      venc.toStream(out, vv)
      wenc.toStream(out, ww)
    }

    def fromStream(in: DataInputStream): K = {
      val tt = tenc.fromStream(in)
      val uu = uenc.fromStream(in)
      val vv = venc.fromStream(in)
      val ww = wenc.fromStream(in)
      apply(tt, uu, vv, ww)
    }
  }

  /** 5-Case with explicit encodings passed by name: for when one of the component types is (or references) `K` */
  class `5-Case-Rec*`[K, T, U, V, W, X](apply: (T, U, V, W, X) => K, unapply: K => Option[(T, U, V, W, X)])
                                       (tenc: => Streamer[T], uenc: => Streamer[U], venc: => Streamer[V], wenc: => Streamer[W], xenc: => Streamer[X])
    extends Streamer[K] {

    def toStream(out: DataOutputStream, t: K): Unit = {
      val Some((tt, uu, vv, ww, xx)) = unapply(t)
      tenc.toStream(out, tt)
      uenc.toStream(out, uu)
      venc.toStream(out, vv)
      wenc.toStream(out, ww)
      xenc.toStream(out, xx)
    }

    def fromStream(in: DataInputStream): K = {
      val tt = tenc.fromStream(in)
      val uu = uenc.fromStream(in)
      val vv = venc.fromStream(in)
      val ww = wenc.fromStream(in)
      val xx = xenc.fromStream(in)
      apply(tt, uu, vv, ww, xx)
    }
  }

  /** @see `1-Case*` */
  class `6-Case-Rec*`[K, T, U, V, W, X, Y](apply: (T, U, V, W, X, Y) => K, unapply: K => Option[(T, U, V, W, X,Y)])
                                          (tenc: => Streamer[T], uenc: => Streamer[U], venc: => Streamer[V], wenc: => Streamer[W], xenc: => Streamer[X], yenc: Streamer[Y])
    extends Streamer[K] {

    def toStream(out: DataOutputStream, t: K): Unit = {
      val Some((tt, uu, vv, ww, xx, yy)) = unapply(t)
      tenc.toStream(out, tt)
      uenc.toStream(out, uu)
      venc.toStream(out, vv)
      wenc.toStream(out, ww)
      xenc.toStream(out, xx)
      yenc.toStream(out, yy)
    }

    def fromStream(in: DataInputStream): K = {
      val tt = tenc.fromStream(in)
      val uu = uenc.fromStream(in)
      val vv = venc.fromStream(in)
      val ww = wenc.fromStream(in)
      val xx = xenc.fromStream(in)
      val yy = yenc.fromStream(in)
      apply(tt, uu, vv, ww, xx, yy)
    }
  }

  /**
    *   A case object is encoded as a single zero byte. It requires no
    *   injection/projection.
    */
  class `Case*`[K](it: K) extends Streamer[K] {
    def toStream(out: DataOutputStream, t: K): Unit = out.writeByte(0)

    def fromStream(in: DataInputStream): K = {
      if (in.readByte()==0) it else
      throw new StreamCorruptedException(s"Bad case object representation: $it expected")
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
  class `Enum*`[E](decoding: Int => E, encoding: E => Int) extends Streamer[E] {
    def toStream(out: DataOutputStream, k: E): Unit = out.writeInt(encoding(k))
    def fromStream(in: DataInputStream): E = decoding(in.readInt())
  }

  /** Essential for `null`-terminated recursive structures, etc.
    * @see StreamerEncodingTests
    */
  implicit object `Null*` extends Streamer[Null] {
    def toStream(out: DataOutputStream, t: Null): Unit = {
      out.writeBoolean(false)
    }

    def fromStream(in: DataInputStream): Null = {
      in.readBoolean()
      null
    }
  }

  /**
    *  Values of type `T` may be be streamed as if they were values of type
    *  `CODE` -- which must itself be streamable. Simply
    *  supply an injection `toCode: T=>CODE` and the corresponding inverse
    *  `fromCode: CODE=>T`.
    *
    */
  class `Encode*`[T, CODE : Streamer](fromCode: CODE=>T, toCode: T=>CODE) extends Streamer[T] {
    val enc = implicitly[Streamer[CODE]]
    def toStream(out: DataOutputStream, k: T): Unit = enc.toStream(out, toCode(k))
    def fromStream(in: DataInputStream): T = fromCode(enc.fromStream(in))
  }

  /** `Set[T}` as `Seq[T]` */
  class `Set*`[T: Streamer] extends `Encode*`[Set[T],Seq[T]](_.toSet, _.toSeq)(`Seq*`[T](implicitly[Streamer[T]]))

  /** Maps encoded as 2-tuple sequences */
  class `PairsMap*`[K : Streamer, V : Streamer] extends Streamer[Map[K,V]] {
    private val enc = `Seq*`[(K, V)](new `2-Tuple*`[K, V])
    def toStream(out: DataOutputStream, t: Map[K, V]): Unit = enc.toStream(out, t.toSeq)
    def fromStream(in: DataInputStream): Map[K, V] = enc.fromStream(in).toMap

    /*
        An alternative is
          implicit object pairEnc extends `2-Tuple*`[K, V]
          implicit object enc extends `Seq*`[(K,V)]
        But, tediously, not the following, which doesn't accept the `2-Tuple*` as implicit (why?)
          implicit object enc extends `Seq*`[(K,V)](new `2-Tuple*`[K, V])

     */
  }

  /** Maps encoded as `Seq*[K]` then `Seq*[V]` */
  class `Map*`[K : Streamer, V: Streamer] extends Streamer[Map[K,V]] {
    private object `keys*` extends `Seq*`[K]
    private object `vals*` extends `Seq*`[V]

    def toStream(out: DataOutputStream, t: Map[K, V]): Unit = {
      val pairs = t.toSeq
      `keys*`.toStream(out, pairs.map(_._1))
      `vals*`.toStream(out, pairs.map(_._2))
    }


    def fromStream(in: DataInputStream): Map[K, V] = {
      val pairs = `keys*`.fromStream(in).zip(`vals*`.fromStream(in))
      new PairMap[K,V](pairs)
    }
  }

  class PairMap[K, V](pairs: Seq[(K, V)]) extends collection.immutable.Map[K, V] {
    def removed(key: K): Map[K, V] = {
      new PairMap(pairs.filter { case (k, _) => k != key })
    }

    def updated[V1 >: V](key: K, value: V1): Map[K, V1] = {
      new PairMap[K, V1]((key, value) +: pairs)
    }

    def get(key: K): Option[V] = {
      (for {(k, v) <- pairs if k == key} yield v) match {
        case Seq(v) => Some(v)
        case _ => None
      }
    }

    def iterator: Iterator[(K, V)] = pairs.iterator
  }

  object PairMap {
    def apply[K,V](pairs: (K,V)*): Map[K,V] = new PairMap(pairs)
  }

  // Notice that class `Enum*`[E] (fromStream: Int => E, code: E => Int) == (E `EncodedAs*` Int)(fromStream, code)


  /**
    *   Implements a small algebra of coercions intended to make the union-type encodings
    *   more scrutable.
    */
  private implicit class Coercion[K,TT](val coerce: K => TT) extends AnyVal {
    def apply(k: K): TT = { coerce(k) }
    def followedBy(g: TT=>Unit): K=>Unit = { k => g(coerce(k)) }
    def orElse[UU](that: Coercion[K,UU]): K=>Unit =
    { k => try coerce(k) catch {
        case _: ClassCastException => that(k)
        case _: MatchError         => that(k) // caused by an identity coercion on a case object
      }
    }
  }

  /** A "case object" cast for use in unions */
  def asObject[K,T](b: T): K=>T = { case k:K @unchecked if k==b => b}

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
    *                  (implicit tenc: Streamer[T], uenc: Streamer[U])
    *                  extends `2union*`[K,T,U] (_.asInstanceOf[T], _.asInstanceOf[U])
    *
    */
  class `2-Union*`[K, T, U](toT: K=>T, toU: K=>U)
                           (implicit tenc: Streamer[T], uenc: Streamer[U]) extends Streamer[K] {

    def toStream(out: DataOutputStream, k: K): Unit = {
      val enc =
          (toT.followedBy { x => out.writeByte(0); tenc.toStream(out, x) }) orElse
          (toU.followedBy { x => out.writeByte(1); uenc.toStream(out, x) })
      enc(k)
    }

    def fromStream(in: DataInputStream): K = {
      in.readByte() match {
        case 0 => tenc.fromStream(in).asInstanceOf[K]
        case 1 => uenc.fromStream(in).asInstanceOf[K]
      }
    }
  }


  /** @see `2-Union*` */
  class `3-Union*`[K, T, U, V](toT: K=>T, toU: K=>U, toV: K=>V)
                             (implicit tenc: Streamer[T], uenc: Streamer[U], venc: Streamer[V]) extends Streamer[K] {

    def toStream(out: DataOutputStream, k: K): Unit = {
      val enc =
          (toT.followedBy { x => out.writeByte(0); tenc.toStream(out, x) }) orElse
          (toU.followedBy { x => out.writeByte(1); uenc.toStream(out, x) }) orElse
          (toV.followedBy { x => out.writeByte(2); venc.toStream(out, x) })
      enc(k)
    }

    def fromStream(in: DataInputStream): K = {
      in.readByte() match {
        case 0 => tenc.fromStream(in).asInstanceOf[K]
        case 1 => uenc.fromStream(in).asInstanceOf[K]
        case 2 => venc.fromStream(in).asInstanceOf[K]
      }
    }
  }

  /** @see `2-Union*` */
  class `4-Union*`[K, T, U, V, W](toT: K => T, toU: K => U, toV: K => V, toW: K => W)
                                (implicit tenc: Streamer[T], uenc: Streamer[U], venc: Streamer[V], wenc: Streamer[W]) extends Streamer[K] {

    def toStream(out: DataOutputStream, k: K): Unit = {
      val enc =
          (toT.followedBy { x => out.writeByte(0); tenc.toStream(out, x) }) orElse
          (toU.followedBy { x => out.writeByte(1); uenc.toStream(out, x) }) orElse
          (toV.followedBy { x => out.writeByte(2); venc.toStream(out, x) }) orElse
          (toW.followedBy { x => out.writeByte(3); wenc.toStream(out, x) })
      enc(k)
    }

    def fromStream(in: DataInputStream): K = {
      in.readByte() match {
        case 0 => tenc.fromStream(in).asInstanceOf[K]
        case 1 => uenc.fromStream(in).asInstanceOf[K]
        case 2 => venc.fromStream(in).asInstanceOf[K]
        case 3 => wenc.fromStream(in).asInstanceOf[K]
      }
    }
  }

  /** @see `2-Union*` */
  class `5-Union*`[K, T, U, V, W, X](toT: K => T, toU: K => U, toV: K => V, toW: K => W, toX: K => X)
                                    (implicit tenc: Streamer[T], uenc: Streamer[U], venc: Streamer[V], wenc: Streamer[W], xenc: Streamer[X]) extends
                                    `5-Union**`[K,T,U,V,W,X](toT, toU, toV, toW, toX)(tenc, uenc, venc, wenc, xenc)


  class `5-Union**`[K, T, U, V, W, X](toT: K => T, toU: K => U, toV: K => V, toW: K => W, toX: K => X)
                                    (tenc: Streamer[T], uenc: Streamer[U], venc: Streamer[V], wenc: Streamer[W], xenc: Streamer[X]) extends Streamer[K] {

    def toStream(out: DataOutputStream, k: K): Unit = {
      val enc =
          (toT.followedBy { x => out.writeByte(0); tenc.toStream(out, x) }) orElse
          (toU.followedBy { x => out.writeByte(1); uenc.toStream(out, x) }) orElse
          (toV.followedBy { x => out.writeByte(2); venc.toStream(out, x) }) orElse
          (toW.followedBy { x => out.writeByte(3); wenc.toStream(out, x) }) orElse
          (toX.followedBy { x => out.writeByte(4); xenc.toStream(out, x) })
      enc(k)
    }

    def fromStream(in: DataInputStream): K = {
      in.readByte() match {
        case 0 => tenc.fromStream(in).asInstanceOf[K]
        case 1 => uenc.fromStream(in).asInstanceOf[K]
        case 2 => venc.fromStream(in).asInstanceOf[K]
        case 3 => wenc.fromStream(in).asInstanceOf[K]
        case 4 => xenc.fromStream(in).asInstanceOf[K]
      }
    }
  }

  /** Encodings for tuples catenate the encodings of the components */
  class `6-Tuple*`[T1: Streamer, T2: Streamer, T3: Streamer, T4: Streamer, T5: Streamer, T6: Streamer]
    extends Streamer[(T1, T2, T3, T4, T5, T6)] {
    val enc1 = implicitly[Streamer[T1]]
    val enc2 = implicitly[Streamer[T2]]
    val enc3 = implicitly[Streamer[T3]]
    val enc4 = implicitly[Streamer[T4]]
    val enc5 = implicitly[Streamer[T5]]
    val enc6 = implicitly[Streamer[T6]]

    def toStream(out: DataOutputStream, v: (T1, T2, T3, T4, T5, T6)): Unit = {
      enc1.toStream(out, v._1)
      enc2.toStream(out, v._2)
      enc3.toStream(out, v._3)
      enc4.toStream(out, v._4)
      enc5.toStream(out, v._5)
      enc6.toStream(out, v._6)
    }

    def fromStream(in: DataInputStream): (T1, T2, T3, T4, T5, T6) = {
      val t1 = enc1.fromStream(in)
      val t2 = enc2.fromStream(in)
      val t3 = enc3.fromStream(in)
      val t4 = enc4.fromStream(in)
      val t5 = enc5.fromStream(in)
      val t6 = enc6.fromStream(in)
      (t1, t2, t3, t4, t5, t6)
    }
  }

  class `5-Tuple*`[T1: Streamer, T2: Streamer, T3: Streamer, T4: Streamer, T5: Streamer]
    extends Streamer[(T1, T2, T3, T4, T5)] {
    val enc1 = implicitly[Streamer[T1]]
    val enc2 = implicitly[Streamer[T2]]
    val enc3 = implicitly[Streamer[T3]]
    val enc4 = implicitly[Streamer[T4]]
    val enc5 = implicitly[Streamer[T5]]

    def toStream(out: DataOutputStream, v: (T1, T2, T3, T4, T5)): Unit = {
      enc1.toStream(out, v._1)
      enc2.toStream(out, v._2)
      enc3.toStream(out, v._3)
      enc4.toStream(out, v._4)
      enc5.toStream(out, v._5)
    }

    def fromStream(in: DataInputStream): (T1, T2, T3, T4, T5) = {
      val t1 = enc1.fromStream(in)
      val t2 = enc2.fromStream(in)
      val t3 = enc3.fromStream(in)
      val t4 = enc4.fromStream(in)
      val t5 = enc5.fromStream(in)
      (t1, t2, t3, t4, t5)
    }
  }

  class `4-Tuple*`[T1: Streamer, T2: Streamer, T3: Streamer, T4: Streamer]
    extends Streamer[(T1, T2, T3, T4)] {
    val enc1 = implicitly[Streamer[T1]]
    val enc2 = implicitly[Streamer[T2]]
    val enc3 = implicitly[Streamer[T3]]
    val enc4 = implicitly[Streamer[T4]]

    def toStream(out: DataOutputStream, v: (T1, T2, T3, T4)): Unit = {
      enc1.toStream(out, v._1)
      enc2.toStream(out, v._2)
      enc3.toStream(out, v._3)
      enc4.toStream(out, v._4)
    }

    def fromStream(in: DataInputStream): (T1, T2, T3, T4) = {
      val t1 = enc1.fromStream(in)
      val t2 = enc2.fromStream(in)
      val t3 = enc3.fromStream(in)
      val t4 = enc4.fromStream(in)
      (t1, t2, t3, t4)
    }
  }

  class `3-Tuple*`[T1: Streamer, T2: Streamer, T3: Streamer]
    extends Streamer[(T1, T2, T3)] {
    val enc1 = implicitly[Streamer[T1]]
    val enc2 = implicitly[Streamer[T2]]
    val enc3 = implicitly[Streamer[T3]]

    def toStream(out: DataOutputStream, v: (T1, T2, T3)): Unit = {
      enc1.toStream(out, v._1)
      enc2.toStream(out, v._2)
      enc3.toStream(out, v._3)
    }

    def fromStream(in: DataInputStream): (T1, T2, T3) = {
      val t1 = enc1.fromStream(in)
      val t2 = enc2.fromStream(in)
      val t3 = enc3.fromStream(in)
      (t1, t2, t3)
    }
  }

  class `2-Tuple*`[T1: Streamer, T2: Streamer]
    extends Streamer[(T1, T2)] {
    val enc1 = implicitly[Streamer[T1]]
    val enc2 = implicitly[Streamer[T2]]

    def toStream(out: DataOutputStream, v: (T1, T2)): Unit = {
      enc1.toStream(out, v._1)
      enc2.toStream(out, v._2)
    }

    def fromStream(in: DataInputStream): (T1, T2) = {
      val t1 = enc1.fromStream(in)
      val t2 = enc2.fromStream(in)
      (t1, t2)
    }
  }



  /** Any `Serializable` type, using object I/O (representations are rather large) */
  class `Serializable*`[T <: Serializable] extends Streamer[T] {
    def toStream(out: DataOutputStream, t: T): Unit = {
      val byteStream = new ByteArrayOutputStream(1024)
      val objOut = new ObjectOutputStream(byteStream)
      objOut.useProtocolVersion(ObjectStreamConstants.PROTOCOL_VERSION_2)
      objOut.writeObject(t)
      objOut.flush()
      val rep = byteStream.toByteArray
      out.writeInt(rep.length)
      out.write(rep)
    }

    def fromStream(in: DataInputStream): T = {
      val length = in.readInt()
      val rep = new Array[Byte](length)
      if (length<in.read(rep)) throw new StreamCorruptedException("Decoding object input (stream too short)")
      val objIn = new ObjectInputStream(new ByteArrayInputStream(rep))
      objIn.readObject().asInstanceOf[T]
    }
  }


  /**
    * Maps `value` to its encoding as an `Array[Byte]`.
    */
  def toByteArray[T: Streamer](value: T): Array[Byte] = {
    val bytes = new ByteArrayOutputStream()
    val enc = implicitly[Streamer[T]]
    val out = new DataOutputStream(bytes)
    enc.toStream(out, value)
    bytes.toByteArray
  }

  /**
    * Maps `encoded` to a `value: T` such that `toByteArray[T](value)==encoded`.
    */
  def fromByteArray[T: Streamer](encoded: Array[Byte]): T = {
    val enc = implicitly[Streamer[T]]
    val in = new DataInputStream(new ByteArrayInputStream(encoded))
    enc.fromStream(in)
  }

  /**
    * Check that a roundtrip encoding/decoding of `value` yields an equivalent `T`.
    * Failure should be expected for structures that have embedded arrays in them
    * and for which there is no defined equality.
    */
  def encodingTest[T: Streamer](value: T): Unit = {
    val encoded = toByteArray[T](value)
    val decoded = fromByteArray[T](encoded)
    (value, decoded) match {
      case (null, null) => println(s"OK null==null (${encoded.length} bytes)")
      case (v: Array[Any], d: Array[Any]) if v.toList == d.toList => println(s"OK ${v.toList.mkString("Array(", ", ", ")")} as an equal array. (${encoded.length} bytes)")
      case (_, _) if value==decoded => println(s"OK ${value.toString.take(70)}... (${encoded.length} bytes)")
      case (_, _) => println(s"**** Roundtrip failure (${encoded.length} bytes)\n**** Encoded: $value\n**** Decoded: $decoded")
    }
  }

}




