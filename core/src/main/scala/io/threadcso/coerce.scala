package io.threadcso

import io.threadcso.process._

import scala.language.implicitConversions

/** To enable implicit coercion of unit-valued expressions to simple processes:
  * `import io.threadcso.coerce.UnitProc`. For example, in the scope of this
  * coercion, the following process expressions are equivalent:
  * {{{
  * (println("foo") || println("bar"))
  *
  * (proc { println("foo") } || proc { println("bar") })
  * }}}
  * CSO neophytes are '''strongly discouraged''' from importing this implicit
  * coercion, for it can lead to confusing error messages from the compiler, and
  * confusing behaviour at run-time.
  */
object coerce {
  import Process.Simple

  /** Implicit coercion from unit-valued expressions to simple processes.
    */
  implicit def UnitProc(body: => Unit): PROC = new Process.Simple(() => body)
}
