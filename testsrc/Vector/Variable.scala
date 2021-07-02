package Vector

/**
  * A variable representing a Vector in 3-space.
  */

class Variable(vx: Double = 0.0, vy: Double = 0.0, vz: Double = 0.0) extends Value(vx, vy, vz)
{

  /** Increment `this` (viewed as a variable) by `that` */
  def +=(that: Value): Unit = {_x += that._x; _y += that._y; _z += that._z }

  /** Decrement `this` (viewed as a variable) by `that` */
  def -=(that: Value): Unit = {_x -= that._x; _y -= that._y; _z -= that._z }

  /** Assign `that` to `this` */
  def :=(that: Value): Unit = {_x= that._x; _y= that._y; _z= that._z }
  def boundAbove(that: Value): Unit = {_x=math.min(_x, that._x); _y=math.min(_y, that._y); _z=math.min(_z, that._z) }
  def boundBelow(that: Value): Unit = {_x=math.max(_x, that._x); _y=math.max(_y, that._y); _z=math.max(_z, that._z) }

  /** Set `this` (viewed as a variable) to zero */
  def setZero(): Unit = {_x=0; _y=0; _z=0 }

  /** Scale the `x` dimension */
  def scaleX(v: Double): Unit = { _x = _x * v }
  /** Scale the `y` dimension */
  def scaleY(v: Double): Unit = { _y = _y * v }
  /** Scale the `y` dimension */
  def scaleZ(v: Double): Unit = { _z = _z * v }

  /** Assign to the `x` dimension */
  def x_=(v:Double): Unit = _x=v
  /** Assign to the `y` dimension */
  def y_=(v:Double): Unit = _y=v
  /** Assign to the `z` dimension */
  def z_=(v:Double): Unit = _z=v
}

object Variable
{ def apply(vx: Double = 0.0, vy: Double = 0.0, vz: Double = 0.0): Variable = new Variable(vx, vy, vz)
}
