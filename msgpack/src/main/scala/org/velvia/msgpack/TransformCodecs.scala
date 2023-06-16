package org.velvia.msgpack

import java.io.DataOutputStream

/**
  *  Codec names changed to be somewhat lighter than those of the original `org.velvia.msgpack`,
  *  but the logic is identical. I gratefully acknowledge the work of velvia.
  */
object TransformCodecs {

  /** Defines a streamer for `T` given one for `CODE` and a bijection `T` <=> `CODE`.
    *
    * An example would be (de)serializing a `Date` by storing its timestamp,
    * which is just a `Long`. This would look like:
    *
    *     import java.util.Date
    *     val c: Codec[Date] = new `EncodeAs*`[Date, Long](_.getTime, new Date(_))
    */
  class `EncodeAs*`[T, CODE: Codec](
    toCODE:   T => CODE,
    fromCODE: CODE => T
  ) extends Codec[T] {

    private val codec1 = implicitly[Codec[CODE]]

    override def pack(out: DataOutputStream, item: T): Unit =
      codec1.pack(out, toCODE(item))

    val unpackFuncMap = {
      val things = codec1.unpackFuncMap.things map { case (byte, func) =>
        byte -> func.andThen(fromCODE)
      }

      FastByteMap[UnpackFunc](things: _*)
    }
  }

  /** Same as `EncodeAs*` but with reversed injection/projection */
  class `Encode*`[T, CODE: Codec](fromCODE: CODE => T, toCODE: T => CODE) extends `EncodeAs*`[T,CODE](toCODE, fromCODE)
}