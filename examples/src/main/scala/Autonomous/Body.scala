import Autonomous._
import display._
import io.threadcso._

import java.awt.{Color, Graphics2D}

class Body(
    var R: Double,
    val position: Position,
    val velocity: Velocity = new Velocity()
) extends Displayable { self =>
  override def toString: String = s"Body($R, $position, $velocity)"

  private var density = 10.0

  private def vol = (4.0 / 3.0) * math.Pi * (R * R * R)

  private[this] var volume = vol

  def setR(radius: Double): Unit = {
    R = radius;
    volume = vol
  }

  /** Current mass of the particle */
  def mass = density * volume

  /** Calculate the position and velocity after time deltaT given the total
    * force on the particle
    */
  def nextState(totalForce: Force): Unit = {
    position += velocity * deltaT
    // Bounce off the edges and lose some momentum if necessary
    if (position.x < R + 5 && velocity.x < 0) {
      velocity.scaleX(-wallBounce);
      position.x = R + 5
    }
    if (position.x + R + 5 > width && velocity.x > 0) {
      velocity.scaleX(-wallBounce);
      position.x = width - R - 5
    }
    if (position.y < R + 5 && velocity.y < 0) {
      velocity.scaleY(-wallBounce);
      position.y = R + 5
    }
    if (position.y + R + 5 > height && velocity.y > 0) {
      velocity.scaleY(-wallBounce);
      position.y = height - R - 5
    }
    // limit speed
    val nextV = velocity + totalForce * deltaT / mass
    if (nextV.magnitude < C) velocity := nextV
  }

  /** Calculate the force attraction on this particle by that particle */
  def attractionTo(that: Body): Force = {
    val bounce = if (this.touches(that)) bodyBounce else 1.0
    val magnitude =
      bounce * G * this.mass * that.mass / (this.position squareTo that.position)
    (this.position directionTo that.position) * magnitude
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

  /** Body's colour on the display: determined by its density
    */
  def color: Color = {
    import math.{log, max, min}
    val greyness = 1.0f - min(0.8, log(max(density, 1.0)) / 15.0).toFloat
    new Color(greyness, greyness, greyness, 0.5f)
  }

  override def paintOn(g: Graphics2D, toPixels: Double => Int) = {
    val W = toPixels(w)
    var H = toPixels(h)
    val X = toPixels(x)
    val Y = toPixels(y)
    g.setColor(color)
    g.fillOval(X, Y, W, H)
    g.setColor(Color.BLACK)
    g.drawOval(X, Y, W, H)
    if (selected) { g.draw3DRect(X, Y, W, H, true) }
    true
  }

  /** Is the body selected? */
  var selected: Boolean = false

  /** Instructions from the environment */
  val instructions: Chan[Message] = OneOne[Message]

  /** This body's controlling process: responding to instructions from the
    * environment.
    */
  val controller: PROC = proc("Body") {
    val others = new collection.mutable.Queue[Body]
    var lastMassExchange, ticks = 0
    repeat {
      instructions ? {
        case Tick =>
          ticks += 1
          val localForce = new ForceVariable()
          // calculate forces between this and the others
          for (other <- others if other ne self) {
            val force =
              (self attractionTo other) * (if (self touches other) bodyBounce
                                           else 1.0)
            // mass exchange
            if (
              (ticks - lastMassExchange > 10) && (self touches other) && (self.mass < other.mass)
            ) {
              lastMassExchange = ticks
              val deltaMass = other.mass * 0.2
              density += (deltaMass / volume)
              other.density -= (deltaMass / other.vol)
            }
            localForce += force
          }
          self.nextState(localForce)
          ()

        case AddBody(body) =>
          others += body
          ()
      }
    }
  }
}

