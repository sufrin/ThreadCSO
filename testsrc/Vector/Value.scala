package Vector

/**
  * Immutable cartesian coordinates in 3-space. Only subclasses have write-access to the underlying representation
  *
  * @see `Vector.Variable`
  */
class Value(protected [Vector] var _x: Double = 0.0, protected [Vector] var _y: Double = 0.0, protected [Vector] var _z: Double = 0.0)
{ override def toString = s"(${_x}, ${_y}, ${_z})"
  /** Scale by `k` */
  def *(k: Double): Value = Value(_x * k, _y * k, _z * k)
  /** Scale by `1/k` */
  def /(k: Double): Value = Value(_x / k, _y / k, _z / k)

  /** Sum of `this` and `that` */
  def +(that: Value): Value = Value(_x + that._x, _y + that._y, _z + that._z)

  /** Difference between `that` and `this` */
  def -(that: Value): Value = Value(_x - that._x, _y - that._y, _z - that._z)

  @inline
  private [this] def sq(d: Double): Double = d*d

  /** Magnitude of this Vector */
  @inline
  def magnitude: Double = Math.sqrt(sq(_x) + sq(_y) + sq(_z))

  /** Direction of this Vector */
  @inline
  def direction: Value = this / magnitude

  /** Square of the distance to `that` */
  @inline
  def squareTo(that: Value): Double = sq(that._x - _x) + sq(that._y - _y) + sq(that._z - _z)

  /** Distance to `that` */
  @inline
  def distanceTo(that: Value): Double = Math.sqrt(squareTo(that))

  @inline
  def directionTo(that: Value): Value =
  { val d = distanceTo(that)
    Value((that._x - _x) / d, (that._y - _y) / d, (that._z - _z) / d)
  }

  @inline def x = _x
  @inline def y = _y
  @inline def z = _z

}

object Value
{ def apply(x: Double = 0.0, y: Double = 0.0, z: Double = 0.0): Value = new Value(x, y, z)
  val Zero = new Value()
}

