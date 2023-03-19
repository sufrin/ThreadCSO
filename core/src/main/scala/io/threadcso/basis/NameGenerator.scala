package io.threadcso.basis

/** Systematic generation of names for classes of CSO object.
  */
class NameGenerator(_kind: String) {
  private val occurs = new java.util.concurrent.atomic.AtomicLong

  /** The kind of object */
  private[threadcso] val kind = _kind

  /** Return <tt>name</tt> if non-null, else an invented name constructed from
    * <tt>kind</tt>.
    */
  private[threadcso] def genName(name: String) = {
    if (name == null) kind + "-" + occurs.getAndIncrement else name
  }

  /** Return an invented name constructed from <tt>kind</tt>.
    */
  private[threadcso] def newName() = {
    kind + "-" + occurs.getAndIncrement.toString
  }

  /** Return an invented name constructed from <tt>kind</tt>.
    */
  private[threadcso] def newName(kind: String) = {
    kind + "-" + occurs.getAndIncrement.toString
  }
}
