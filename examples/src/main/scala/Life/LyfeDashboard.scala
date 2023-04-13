



import display._
import io.threadcso._

/**
  *
  * A window frame that includes a `Display` used to display a collection of `Displayable`
  * `Body`s as well as a few controls that determine the (mostly physical) characteristics
  * of the bodies.
  *
  * `allBodies` is the (mutable) collection of bodies that is to be displayed on each frame.
  * The collection may change between frames.
  *
  * `title` is the title to be placed on the window.
  *
  */
class LyfeDashboard[Body<:Displayable](val allBodies: collection.mutable.Queue[Body], val title: String="", var width: Int = 1200, var height: Int = 800) {

  var FPS: Int    = 10 // Frames/second

  /** Whether the simulation is running or not */
  var running = false


  import widgets._

  /** Messages arriving from bodies */
  val fromDisplay = N2NBuf[Event[Body]](0, writers = 0, readers = 0)

  /** Permission to draw a single frame */
  val singleFrame = io.threadcso.lock.primitive.UnitChan() // BooleanSemaphore(running)

  // a change in state to true signals the display controller
  val run = checkBox("Running", running) { state =>
    running = state
    if (running) singleFrame ! ()
  } withTip ("Space also starts and stops")

  def setRunning(on: Boolean): Unit = {
    running = on
    run.setSelected(on)
    if (running) singleFrame ! ()
  }

  def refresh(): Unit = {
    display.draw(syncWait = false)
  }

  /** Accumulated overrun in nanoseconds since the last FPS change */
  var overRun: Double = 0.0
  var overCount: Int = 0

  val overruns = textField("", 10) { _ => }.withTitledBorder("Average overrun μs").withTip("Average frame overrun time since the last FPS adjustment")
  val reports = textField("", 60) { _ => }.withTitledBorder("Reports")

  val fps = spinner(FPS, 0, 100, 5) {
    value =>
        FPS       = value + 1
        overRun   = 0.0
        overCount = 0
        overruns.setText("None")
  } withTitledBorder ("FPS") withTip ("Target number of frames per (real) second")

  def reportOverrun(behind: Double): Unit = {
    overRun += behind
    overCount = (overCount + 1) % 100
    if (overCount==1) {
      overruns.setText(f"${overRun/100E6}%3.2f")
      overRun = 0.0
    }
  }

  def report(message: String): Unit = {
    reports.setText(message)
  }

  def controls = row(
    run.withEtchedBorder (), fps, hGlue
  ).withEtchedBorder()

  val display = new Display[Body](
    allBodies,
    title,
    width = width,
    height = height,
    events = fromDisplay,
    keys = fromDisplay,
    components = null, // No response to resizings
    motions = fromDisplay,
    North = controls,
    South = row(reports, hGlue, overruns)
  )

}
