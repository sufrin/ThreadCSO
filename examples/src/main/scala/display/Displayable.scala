package display
import java.awt.{Color, Graphics2D}

/** Properties of an object that can be displayed.
  */
trait Displayable {

  /** Bottom left coordinate  in model units */
  def x: Double

  /** Bottom left coordinate in model units */
  def y: Double

  /** Width  in model units */
  def w: Double

  /** Height in model units */
  def h: Double

  /** Reference colour: need not be meaningful in all applications */
  def color: Color = Color.RED

  /** Does the displayable contain the given point (in model coordinates) By
    * default the displayable is identified with its bounding box, but a more
    * subtle computation may be used
    */
  def contains(px: Double, py: Double): Boolean = {
    val vx = x
    val vy = y
    vx <= px && px <= vx + w && vy <= py && py <= vy + h
  }

  /** Has the displayable been selected (change appearance) */
  def selected: Boolean

  /** Paint this displayable on `g` and return true; else return false and let the display paint the default */
  def paintOn(g: Graphics2D, toPixels: Double=>Int): Boolean = false
}
