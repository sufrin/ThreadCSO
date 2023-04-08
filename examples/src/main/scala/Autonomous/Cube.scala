import Autonomous._
import display._
import io.threadcso._

import java.awt.{Color, Graphics2D}



class Cube(
            var _R: Double,
            val position: Position,
            val velocity: Velocity = new Velocity()
          ) extends Body { self =>
  override def toString: String = f"Cube($R%3.2f@$density%3.2f=$mass%3.2g, $position%s, $velocity%s)"

  val massive = true

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

  /** Calculate the position and velocity after time deltaT given the total
    * force on the particle
    */
  def nextState(totalForce: Force): Unit = {
    if (massive) {} else {
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
      val nextV = velocity + (totalForce * deltaT) / mass
      velocity := (if (velocity.magnitude < C) nextV else velocity.direction * C)
    }
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

  override def paintOn(g: Graphics2D, toPixels: Double=>Int) = {
      val W = toPixels(w)
      var H = toPixels(h)
      val X = toPixels(x)
      val Y = toPixels(y)
      g.setColor(color)
      g.fillRect(X, Y, W, H)
      if (selected) {
        g.setColor(Color.BLACK)
        g.drawRect(X, Y, W, H)
      } else {
        g.setColor(if (massive) Color.GREEN else Color.RED)
        g.drawRect(X, Y, W, H)
      }
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
  val controller: PROC = proc("Cube") {
    // bodies of relevance tp this body
    val others = new collection.mutable.Queue[Body]
    var lastMassExchange, ticks = 0
    repeat (true) {
      instructions ? {
        case Tick =>
          ticks += 1
          if (massive) {} else {
            val localForce = new ForceVariable()
            // calculate forces between this and the others
            for (other <- others if other ne self) {
              val force = (self attractionTo other)
              // mass exchange
              if (massExchange) {
                if ((ticks - lastMassExchange > 10) && (self touches other) && (self.mass < other.mass)) {
                  lastMassExchange = ticks
                  val deltaMass = other.mass * 0.33
                  density += (deltaMass / volume)
                  other.density -= (deltaMass / other.vol)
                }
              }
              localForce += force
            }
            self.nextState(localForce)
          }


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