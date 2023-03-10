import java.awt._
import java.awt.event._

/** Specialized AWT Frame to display a grid
  */
class Display(N: Int, a: Array[Array[Boolean]]) extends Frame {
  // Define some constants
  private val blockSize = 6
  private val padding = 1
  private val gridSize = blockSize + 2 * padding

  // Set up the display
  private val pane = new ScrollPane()
  pane.setSize(N * gridSize, N * gridSize)
  private val board = new Board()
  pane.add(board)
  this.add(pane, "Center")
  this.pack()
  this.setVisible(true)
  this.setTitle("Life")
  this.setSize(N * gridSize, N * gridSize)
  // Stop the program when the window is closed
  this.addWindowListener(new WindowAdapter() {
    override def windowClosing(e: WindowEvent): Unit = { io.threadcso.exit(0) }
  })

  // Fill in all the squares
  def draw = {
    for (i <- 0 until N) {
      for (j <- 0 until N) {
        if (a(i)(j)) board.drawAt(j, i) else board.blankAt(j, i)
      }
    }
  }

  override def paint(g: Graphics) = draw

  class Board extends Component {
    // Define colours
    val backgroundColor = Color.gray.brighter
    val blockColor = Color.black

    // Paint the square at (x,y) in colour c
    @inline private def paintAt(x: Int, y: Int, c: Color) = {
      val g = getGraphics()
      g.setColor(c)
      g.fillRect(
        x * gridSize + padding,
        y * gridSize + padding,
        blockSize,
        blockSize
      )
    }

    // Draw a piece
    def drawAt(x: Int, y: Int) = paintAt(x, y, blockColor)

    // Draw a blank square
    def blankAt(x: Int, y: Int) = paintAt(x, y, backgroundColor)
  }

}
