import java.awt._;
import javax.swing.{JFrame, JComponent}
import scala.collection.mutable.Set
import io.threadcso._

object BoidDisplay {
  // Synchronizing at the end of frame drawing?
  private var synchronizing = true

  def Synchronizing: Boolean = synchronizing

  /* Set whether synchronizing or not, avoiding race with frameWait */
  def Synchronizing(sync: Boolean) = { synchronizing = sync }

  /* Await a signal for the end of frame drawing from the GUI thread, using the given semaphore */
  private def frameWait(sync: Semaphore) = {
    if (synchronizing) sync.down()
  }

  /* Signal the end of frame drawing from the GUI thread, using the given semaphore */
  private def frameSignal(sync: Semaphore) = sync.up() // unconditional

  /* Set antialiasing hints when drawing */
  var antiAlias = true

}

class BoidDisplay(boids: Set[Boid]) extends JFrame {
  import BoidDisplay._
  // Constants
  private val gridSize = 1.0 // pixels per unit length

  // dimensions of triangle representing boid (pixels)
  private val boidLength = 15.0 // length of triangle
  private val boidBase = 5.0 // half width of base of triangle

  // arena size (pixels)
  private val bwidth = (BBox.xSize * gridSize).toInt
  private val bheight = (BBox.ySize * gridSize).toInt

  // Set up the display
  val board = new Board()
  board.setPreferredSize(new Dimension(bwidth, bheight))

  this.getContentPane.add(board, "Center");
  this.pack()
  this.setVisible(true)

  val sync = BooleanSemaphore(available = false)

  // Redraw the display from the current state of Boids
  // Must be called /off/ the GUI thread; invokes paint /on/ the GUI thread; then waits for its completion
  def draw() = {
    board.paintImmediately(0, 0, bwidth, bheight);
    frameWait(sync)
  }

  setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
  setTitle("Boids")

  class Board extends JComponent {
    val boidColor = Color.blue

    // Called /on/ the GUI thread
    override def paint(gr: Graphics) = {
      import RenderingHints._
      val g = gr.asInstanceOf[Graphics2D]

      if (antiAlias) {
        g.setRenderingHint(KEY_TEXT_ANTIALIASING, VALUE_TEXT_ANTIALIAS_ON)
        g.setRenderingHint(KEY_ANTIALIASING, VALUE_ANTIALIAS_ON)
      }
      g.setColor(boidColor)
      for (b <- boids) paintBoid(b, g)
      frameSignal(sync)
    }

    // Display a boid
    private def paintBoid(boid: Boid, g: Graphics) = {
      // Calculate coordinates of triangle representing boid
      val xPoints = new Array[Double](3)
      val yPoints = new Array[Double](3)
      val sin = Math.sin(boid.direction)
      val cos = Math.cos(boid.direction)
      xPoints(0) = boid.x + boidLength * sin;
      yPoints(0) = boid.y + boidLength * cos;
      xPoints(1) = boid.x - boidBase * cos;
      yPoints(1) = boid.y + boidBase * sin;
      xPoints(2) = boid.x + boidBase * cos;
      yPoints(2) = boid.y - boidBase * sin;
      g.fillPolygon(xPoints.map(x => x.toInt), yPoints.map(y => y.toInt), 3);
    }
  }
}
