package io.threadcso.basis

/** A mixin to support the naming of objects during or after construction.
  */
trait Named[+T] {
  self: T =>

  /** Return the name of the object */
  def name: String = _name

  /** The name of the object */
  private var _name: String = "<anonymous>"

  /** Set the name of this object and return it */
  def withName(__name: String): T = {
    _name = __name
    this
  }

  /** Discover the name generator */
  def nameGenerator: NameGenerator

  /** Set the name using the name generator */
  def setName(name: String): Unit = {
    _name = nameGenerator.genName(name)
  }

  override def toString: String = name
}
