package display
import io.threadcso._
import javax.swing._
import java.awt.{Color, RenderingHints, Graphics, Graphics2D, Dimension}
import java.awt.event._
import widgets.Widget

/** A display used to draw the given iterable collection of `Displayables`
  * whenever `draw` is invoked. `width`, `height` are in (abstract) units of
  * length; these are scaled by `Display.pixelsPerUnitLength` in order to
  * calculate the width and height of the display in pixels. If `sync` is
  * non-null, then it is a semaphore used to ensure that `draw` returns only
  * after the complete drawing has been rendered by the GUI thread.
  *
  * Iterations over `displayables` are caused by (and are executed during)
  * invocations of `draw` and of `at`. Such iterations are only as thread-safe
  * as the iterators generated by `displayables`, so it is recommended to avoid
  * changes to `displayables` that affect its iterators during such invocations.
  *
  *   1. Mouse events (including mouse wheel / touchpad events) are sent to the
  *      `events` port if it is non-null.
  *   1. Mouse motion events are sent to the `motions` port if it is non-null.
  *   1. Key events are sent to the `keys` port if it is non-null.
  *   1. It is strongly recommended that the `motions` port belong to a buffered
  *      channel if it is non-null.
  *   1. The ports may (but need not) belong to the same channel.
  *   1. If `keys` is non-null then when the mouse enters the display part of
  *      the window, the window is given the keyboard focus. It keeps the focus
  *      until it is "taken" by some other window on the screen.
  *
  * The parameters `North, South, East, West` specify components to be placed
  * above, below, to the right, and to the left of the component on which the
  * displayables are to be displayed.
  */
class Display[D <: Displayable](
    displayables: Iterable[D],
    title: String = "Display",
    width: Double = Display.width,
    height: Double = Display.height,
    val sync: Semaphore =
      BooleanSemaphore(available = false, name = "Display.sync"),
    val events: !![Event[D]] = null,
    val motions: !![Event[D]] = null,
    val keys: !![Event[D]] = null,
    val components: !![Event[D]] = null,
    val North: Widget[JComponent] = null,
    val South: Widget[JComponent] = null,
    val East: Widget[JComponent] = null,
    val West: Widget[JComponent] = null
) extends JFrame {

  import Display._

  // display frame  dimensions (pixels)
  private var frameWidth = toPixels(width)
  private var frameHeight = toPixels(height)

  def setDimensions(width: Double, height: Double): Unit = {
    frameWidth = toPixels(width)
    frameHeight = toPixels(height)
  }

  /** The frame within which `display.Displayable`s are painted */
  private[this] val displayFrame = new DisplayFrame()
  displayFrame.setPreferredSize(new Dimension(frameWidth, frameHeight))

  def setupGUI(): Unit = {
    this.getContentPane
      .add(Display.this.displayFrame.withEtchedBorder(), "Center")
    if (North != null) this.getContentPane.add(North, "North")
    if (South != null) this.getContentPane.add(South, "South")
    if (East != null) this.getContentPane.add(East, "East")
    if (West != null) this.getContentPane.add(West, "West")
    this.pack()
    this.setVisible(true)
    this.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
    this.setTitle(title)
  }
  javax.swing.SwingUtilities.invokeLater(new Runnable {
    def run() = setupGUI()
  })

  /** Set antialiasing hints (if specified) on the given `Graphics2D`. */
  @inline private[this] def antiAliasingHints(g: Graphics2D): Unit =
    if (antiAlias) {
      import RenderingHints._
      g.setRenderingHint(KEY_TEXT_ANTIALIASING, VALUE_TEXT_ANTIALIAS_ON)
      g.setRenderingHint(KEY_ANTIALIASING, VALUE_ANTIALIAS_ON)
    }

  /** Await the end of frame drawing, using the given semaphore */
  @inline private[this] def frameWait = if (sync != null) sync.down()

  /** Signal the end of frame drawing, using the given semaphore */
  @inline private[this] def frameSignal =
    if (sync != null) sync.up() // unconditional

  /** Pause the drawing thread: this must be called from a thread distinct from
    * that generating calls to `draw`.
    */
  def pause: Unit = frameWait

  /** Allow the drawing thread to proceed: this must be called from a thread
    * distinct from that generating calls to `draw`.
    */
  def resume: Unit = frameSignal

  /** Start the redrawing of the display from the current state of displayables,
    * and (if `sync` is non-null) await the signal that the display has been
    * finished. This method must be called /off/ the GUI thread.
    */
  def draw(syncWait: Boolean = true) = {
    displayFrame.repaint(0, 0, frameWidth, frameHeight)
    if (syncWait) frameWait
  }

  /** Start the redrawing of the given sub-rectangle of the display from the
    * current state of displayables, and (if `sync` is non-null) await the
    * signal that the display has been finished. This method must not be called
    * from the GUI thread.
    */
  def draw(x: Double, y: Double, w: Double, h: Double) = {
    displayFrame.repaint(toPixels(x), toPixels(y), toPixels(w), toPixels(h));
    frameWait
  }

  /** If the display is synchronized (`sync` is non-null) then return the
    * subcollection of `displayable` that contained the point `(x,y)` during the
    * most recent completed invocation of `draw`. If not synchronized, or not
    * `synchronous` then return the the subcollection of displayable` that
    * contains the point `(x, y)`
    */
  def at(x: Double, y: Double, synchronous: Boolean = true): List[D] = {
    try {
      if (synchronous) pause
      val contains =
        for (displayable <- displayables if (displayable.contains(x, y)))
          yield displayable
      contains.toList
    } finally { if (synchronous) resume }
  }

  /** Paint the given `display.Displayable` using the given `Graphics` context.
    * When `draw` has been called, this is eventually invoked for each
    * `display.Displayable` owned by the display. If the given `displayable` can
    * paint itself on `g` then it does so, otherwise a default image is drawn.
    */
  protected def paintDisplayable(displayable: D, g: Graphics2D) = {
    if (displayable.paintOn(g, toPixels)) {} else {
      val W = toPixels(displayable.w)
      var H = toPixels(displayable.h)
      val x = toPixels(displayable.x)
      val y = toPixels(displayable.y)
      g.setColor(displayable.color)
      g.fillOval(x, y, W, H)
      if (displayable.selected) {
        g.setColor(Color.BLACK);
        g.draw3DRect(x, y, W, H, true)
      }
    }
  }

  private[this] class DisplayFrame extends Widget[JComponent] {

    locally {
      setDoubleBuffered(true)
    }

    override def paint(gr: Graphics) = {
      super.paint(gr)
      val g = gr.asInstanceOf[Graphics2D]
      antiAliasingHints(g)
      for (displayable <- displayables) paintDisplayable(displayable, g)
      frameSignal
    }

    if (events != null) {
      addMouseListener(mouseListen); addMouseWheelListener(mouseListen)
    }
    if (motions != null) addMouseMotionListener(mouseListen)
    if (keys != null) addKeyListener(keyListen)
    if (components != null) addComponentListener(componentListen)

    private[this] object componentListen extends ComponentListener {
      def componentHidden(ev: ComponentEvent): Unit = {
        components ! ComponentHidden(detail(ev))
      }
      def componentMoved(ev: ComponentEvent): Unit = {
        components ! ComponentMoved(detail(ev))
      }
      def componentResized(ev: ComponentEvent): Unit = {
        components ! ComponentResized(detail(ev))
      }
      def componentShown(ev: ComponentEvent): Unit = {
        components ! ComponentShown(detail(ev))
      }
      def detail(jdk: ComponentEvent): ComponentEventDetail[D] =
        ComponentEventDetail[D](jdk, pixelsPerUnitLength)
    }

    private[this] object mouseListen
        extends MouseListener
        with MouseMotionListener
        with MouseWheelListener {
      def mouseExited(e: MouseEvent): Unit = events ! Exited(detail(e))
      def mousePressed(e: MouseEvent): Unit = events ! Pressed(detail(e))
      def mouseReleased(e: MouseEvent): Unit = events ! Released(detail(e))
      def mouseEntered(e: MouseEvent): Unit = {
        if (keys != null) requestFocusInWindow(); events ! Entered(detail(e))
      }
      def mouseClicked(e: MouseEvent): Unit = events ! Clicked(detail(e))
      def mouseMoved(e: MouseEvent): Unit = events ! Moved(detail(e))
      def mouseDragged(e: MouseEvent): Unit = events ! Dragged(detail(e))
      def mouseWheelMoved(e: MouseWheelEvent): Unit =
        events ! Wheel(new MouseWheelEventDetail[D](e, pixelsPerUnitLength))
      def detail(jdk: MouseEvent): MouseEventDetail[D] =
        new MouseEventDetail[D](jdk, pixelsPerUnitLength)
    }

    object keyListen extends KeyListener {
      override def keyPressed(e: KeyEvent): Unit = keys ! KeyPressed(detail(e))
      override def keyReleased(e: KeyEvent): Unit =
        keys ! KeyReleased(detail(e))
      override def keyTyped(e: KeyEvent): Unit = keys ! KeyTyped(detail(e))
      def detail(jdk: KeyEvent): KeyEventDetail[D] =
        new KeyEventDetail[D](jdk, pixelsPerUnitLength)
    }

  }

}

object Display {

  /** Use antialiasing hints when drawing */
  var antiAlias = true

  /** Default width of each display in length units */
  var width = 1000.0

  /** Default height of each display in length units */
  var height = 700.0

  /** Pixels per unit length */
  var pixelsPerUnitLength = 1.0

  @inline def toPixels(n: Double): Int =
    (n * pixelsPerUnitLength).toInt

  @inline def toLength(n: Int): Double =
    (n / pixelsPerUnitLength).toDouble
}
