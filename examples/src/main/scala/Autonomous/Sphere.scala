import Message._
import Types._
import io.threadcso._

import java.awt.{Color, Graphics2D}

class Sphere(
            var _R:        Double,
            val position:  Position,
            val velocity:  Velocity = new Velocity(),
            val context:   UserInterface,
          ) extends Body { self =>

  override def toString: String = f"Sphere($R%3.2f@$density%3.2f=$mass%3.2g, $position%s, $velocity%s)"

  private var _density = 512.0
  def density: Double = _density

  def density_=(d: Double): Unit =
    _density = minDensity max (d min maxDensity)

  def R: Double = _R

  def R_=(radius: Double): Unit = {
    val v   = vol
    _R      = radius max 1.0
    volume  = vol
    density = density * (v / volume)
  }

  def vol = (4.0 / 3.0) * math.Pi * (R * R * R)

  private[this] var volume = vol

  var force: Force = Vector.Value.Zero

  /** Calculate the position and velocity after time deltaT given the total
    * force on the particle
    */
  def nextState(totalForce: Force): Unit = {
    force = totalForce
    position += velocity * context.deltaT
    // Bounce off the edges and lose some momentum if necessary
    if (position.x < R + 5 && velocity.x < 0) {
      velocity.scaleX(-context.wallBounce);
      position.x = R + 5
    }
    if (position.x + R + 5 > context.width && velocity.x > 0) {
      velocity.scaleX(-context.wallBounce);
      position.x = context.width - R - 5
    }
    if (position.y < R + 5 && velocity.y < 0) {
      velocity.scaleY(-context.wallBounce);
      position.y = R + 5
    }
    if (position.y + R + 5 > context.height && velocity.y > 0) {
      velocity.scaleY(-context.wallBounce);
      position.y = context.height - R - 5
    }
    // limit speed
    val nextV = velocity + (totalForce * context.deltaT) / mass
    velocity := (if (velocity.magnitude<context.C) nextV else velocity.direction*context.C)
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
      g.setColor(Color.RED)
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
  val controller: PROC = proc("Sphere") {
    val others = new collection.mutable.Queue[Body]
    var lastMassExchange, ticks = 0
    repeat (true) {
      instructions ? {
        case DeltaD(delta: Double) =>
          density = density + delta
        case DeltaR(delta: Double) =>
          R = R + delta
        case DeltaV(factor: Double) =>
          velocity := velocity * factor

        case Tick =>
          ticks += 1
          val localForce = new ForceVariable()
          // calculate forces between this and the others
          for (other <- context.allBodies if other ne self) {
            val force = (self attractionTo other)
            // mass exchange
            if (context.massExchange) {
              if ((ticks - lastMassExchange > 10) && (self touches other) && (self.mass < other.mass)) {
                lastMassExchange = ticks
                val deltaMass = other.mass * 0.2
                density += (deltaMass / volume)
                other.instructions!DeltaD(-deltaMass / other.vol)
              }
            }
            localForce += force
          }
          self.nextState(localForce * Autonomous.GUI.G)
          ()

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