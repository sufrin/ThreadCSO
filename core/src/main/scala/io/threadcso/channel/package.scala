package io.threadcso

/** Specifies primitive (non-alternation-capable) ports and transport; and
  * implements several channel types.
  */
package object channel {
  type ??[+T] = InPort[T]
  type !![-T] = OutPort[T]
}
