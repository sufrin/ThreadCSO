import Message._
import Types._
import display.Displayable
import io.threadcso.{!!, PROC}

import java.awt.Color

trait Body extends Displayable {
  def R:        Double

  val position: Position
  val velocity: Velocity
  def force:    Force      // last calculated force

  def touches(that: Body): Boolean

  def density:  Double
  def vol:      Double
  def mass:     Double = density * vol

  def selected: Boolean
  def selected_=(sel: Boolean): Unit

  val maxDensity = 50000.0
  val minDensity = 0.1
  val quant = (maxDensity - minDensity)

  def color: Color = {
    import math.{abs, log, max, min}
    val greyness = (1.0f - (0.3+density/quant).toFloat) min 1.0f max 0.0f
    new Color(greyness, greyness, 0.5f, 0.3f)
  }

  /** Calculate the force attraction on this particle by that particle: multiple of G */
  def attractionTo(that: Body): Force = {
    val magnitude =
      this.mass * that.mass / (this.position squareTo that.position)
    (this.position directionTo that.position) * magnitude
  }

  /** Move to the next state */
  def nextState(force: Force): Unit

  /**
    *   The controlling PROC of this body
    */
  val controller:   PROC

  /**
    *   The port on which to instruct this body
    */
  val instructions: !![Message]

}
