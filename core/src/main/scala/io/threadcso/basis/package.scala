package io.threadcso

/** A home for types that are needed more or less pervasively.
  * {{{
  * @author Bernard Sufrin, Oxford
  * \$Revision: 228 $
  * \$Date: 2016-03-04 16:11:56 +0000 (Fri, 04 Mar 2016)
  * }}}
  *
  *   - [[io.threadcso.basis.NameGenerator]] -- Systematic generation of names
  *     for classes of CSO objects
  *   - [[io.threadcso.basis.Named]] -- Mixin to name objects during or after
  *     construction \-
  */

package object basis {

  /** Units of time expressed as nanoseconds: (eg) sleep(3*Day+14*Hour) */
  type Nanoseconds = Long

  /** Units of time expressed as milliseconds: */
  type Milliseconds = Long

  /** Implicit class to print thread names and identities
    */
  implicit class Identity(thr: Thread) {
    def identity: String =
      if (thr == null) "?" else s"${thr.getName}#${thr.threadId}"
  }

  /** Return `coerce(`''text''`)` for a property specified on the scala command
    * line by `-Dname=`''text''. If there is no such property then return
    * `orelse`.
    */
  def getPropElse[T](name: String, coerce: String => T)(orelse: T): T = {
    val prop = java.lang.System.getProperty(name)
    if (prop == null) orelse else coerce(prop)
  }

  def getPropElse(name: String)(orelse: String): String = {
    val prop = java.lang.System.getProperty(name)
    if (prop == null) orelse else prop
  }

  /** Wait until `deadline` for `condition` to become true. If it became true
    * before the deadline then the result is the time remaining when it became
    * true. Otherwise the result will be negative, and representing the time
    * after the deadline when deadline expiry was noticed.
    *
    * @param blocker
    *   the object to be reported as the blocker by debuggers
    * @param deadline
    *   the deadline in nanoseconds
    * @param condition
    *   the condition
    * @return
    *   Nanoseconds remaining when the condition became true or when the
    *   deadline expired (possibly negative)
    */
  @inline def parkUntilDeadlineOr(
      blocker: AnyRef,
      deadline: Nanoseconds,
      condition: => Boolean
  ): Nanoseconds = {
    var left = deadline - System.nanoTime
    while (left > 0 && !condition) {
      java.util.concurrent.locks.LockSupport.parkNanos(blocker, left)
      left = deadline - System.nanoTime
    }
    // left<=0 || condition
    return left
  }

  /** Equivalent to `parkUntilDeadline(blocker, timeOut+System.nanoTime,
    * condition)`
    */
  @inline def parkUntilElapsedOr(
      blocker: AnyRef,
      timeOut: Nanoseconds,
      condition: => Boolean
  ): Nanoseconds = {
    val deadline = timeOut + System.nanoTime
    var left = timeOut
    while (left > 0 && !condition) {
      java.util.concurrent.locks.LockSupport.parkNanos(blocker, left)
      left = deadline - System.nanoTime
    }
    // left<=0 || condition
    return left
  }

}
