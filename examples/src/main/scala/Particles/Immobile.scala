import Message._
import Types._
import io.threadcso._

import java.awt.{Color, Graphics2D}


class Immobile(
                var _R: Double,
                val position: Position,
                val velocity: Velocity = new Velocity(),
                val context:  UserInterface
              ) extends Body { self =>
  override def toString: String = f"Immobile($R%3.2f@$density%3.2f=$mass%3.2g, $position%s, $velocity%s)"

  val fixed = true

  private var _density = 16.0
  def density: Double = _density

  def density_=(d: Double): Unit =
    _density = minDensity max (d min maxDensity)

  def R: Double = _R

  def R_=(radius: Double): Unit = {
    val v = vol
    _R = radius max 1.0
    volume = vol
    density = density * (v / volume)
  }

  def vol: Double = R * R * R

  private[this] var volume = vol

  var force: Force = Vector.Value.Zero

  /** Calculate the position and velocity after time deltaT given the total
    * force on the particle
    */
  def nextState(totalForce: Force): Unit = {
    force = totalForce
  }

  /** When the particles touch */
  def touches(that: Body): Boolean =
    (this.position distanceTo that.position) < (this.R + that.R)

  /** Change density */
  def changeDensity(m: Double): Unit = density *= m

  /** Display attribute */
  def x: Double = position.x - R

  /** Display attribute */
  def y: Double = position.y - R

  /** Display attribute */
  def w: Double = 2 * R

  /** Display attribute */
  def h: Double = 2 * R

  override def paintOn(g: Graphics2D, toPixels: Double=>Int) = {
      val W = toPixels(w)
      var H = toPixels(h)
      val X = toPixels(x)
      val Y = toPixels(y)
      g.setColor(color)
      g.fillOval(X, Y, W, H)
      if (selected) {
        g.setColor(Color.BLACK)
        g.drawRect(X, Y, W, H)
      }
      g.setColor(Color.GREEN)
      g.drawOval(X, Y, W, H)
      true
  }

  /** Is the body selected? */
  var selected: Boolean = false

  /** Instructions from the environment: buffered to avoid deadlock */
  val instructions: Chan[Message] = N2NBuf[Message](100, -1, -1)

  /**
    * This body's controlling process: responding to
    * instructions from the environment.
    */
  val controller: PROC = proc("Immobile") {
    // bodies of relevance tp this body
    val others = new collection.mutable.Queue[Body]
    var ticks = 0
    repeat (true) {
      instructions ? {
        case DeltaD(delta: Double) =>
          density = density + delta
        case DeltaR(delta: Double) =>
          R = R + delta
        case DeltaV(factor: Double) =>
          // We have no velocity to change
        case Tick =>
          ticks += 1
          val localForce = new ForceVariable()
          // calculate forces on this from the others
          for (other <- context.allBodies if other ne self)
              localForce += self attractionTo other
          // In case we want to show the force on us
          self.nextState(localForce * Autonomous.GUI.G)

        case AddBody(body) =>
          others += body
          ()


        case RemoveBody(body) =>
          others -= body
          ()

      }
    }
    println(s"$this deleted")
  }
}