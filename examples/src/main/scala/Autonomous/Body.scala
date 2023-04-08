import Autonomous._
import display.Displayable
import io.threadcso.{!!, PROC}

import java.awt.Color

trait Body extends Displayable {
  def R_=(radius: Double): Unit
  def R: Double
  def nextState(totalForce: Force): Unit
  def touches(that: Body): Boolean
  val position: Position
  val velocity: Position
  def density: Double
  def density_=(d: Double): Unit
  def vol: Double
  def mass: Double = density * vol
  val instructions: !![Message]
  val controller: PROC
  def selected: Boolean
  def selected_=(sel: Boolean): Unit

  val maxDensity = 50000.0
  val minDensity = 0.1
  val quant = (maxDensity - minDensity)

  val massExchange = false

  def color: Color = {
    import math.{abs, log, max, min}
    val greyness = (1.0f - (log(density)/log(quant))).toFloat min 1.0f max 0.0f
    new Color(greyness, greyness, greyness, 0.3f)
  }

}