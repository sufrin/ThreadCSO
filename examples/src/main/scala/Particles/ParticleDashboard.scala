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
class ParticleDashboard[Body<:Displayable](val allBodies: collection.mutable.Queue[Body], val title: String="") {

  var deltaT: Double = 1.0
  var bodyBounce: Double = -0.9
  var wallBounce: Double = 0.9
  var width: Int = 1200
  var height: Int = 800
  var scale: Int = -7

  def scaleG: Double = 6.79 * math.pow(10.0, scale)

  var G: Double = scaleG // Gravitational constant, dimensions are m^3/k/s^2
  var FPS: Int = 50 // Frames/second
  /** Maximum speed */
  var C: Double = 30.0

  /** Whether the simulation is running or not */
  var running = false


  import widgets._

  /** Messages arriving from bodies */
  val fromDisplay = N2NBuf[Event[Body]](0, writers = 0, readers = 0)

  /** Permission to draw a single frame */
  val singleFrame = io.threadcso.lock.primitive.UnitChan() // BooleanSemaphore(running)

  var repelling = 1.0

  var massExchange: Boolean = false

  val gravity = row(
    label("6.79E"),
    spinner(scale, -24, 2, 1) { value =>
      scale = value; G = scaleG * repelling
    }.alignLeft
  ) withTitledBorder ("Gravity") withTip ("Exponent of gravitational constant")

  val repel = col(checkBox("  ", false) { state =>
    repelling = if (state) -1.0 else 1.0; G = math.abs(G) * repelling
  }) withTitledBorder ("Repel") withTip ("Negate gravitational constant (forces are repulsive)")

  val mx = col(checkBox("  ", massExchange) { state =>
    massExchange = state
  }) withTitledBorder ("MX") withTip ("Mass exchange on collisions")

  val wBounce = spinner(wallBounce, -5.0, 5.0, 0.1) { value =>
    wallBounce = value
  } withTitledBorder ("Wall") withTip ("Coefficient of restitution of the wall")

  val bBounce = spinner(bodyBounce, -50.0, 50.0, 0.1) { value =>
    bodyBounce = value
  } withTitledBorder ("Bounce") withTip ("Bounciness of other bodies")

  val time = spinner(deltaT, 0.0, 50.0, 0.25) { value =>
    deltaT = value
  } withTitledBorder ("∂T") withTip ("Simulated time increment")

  val lightspeed = spinner(C, 10.0, 200.0, 5) { value =>
    C = value
  } withTitledBorder ("C") withTip ("Maximum simulated velocity")

  // a change in state to true signals the display controller
  val run = checkBox("Running", running) { state =>
    running = state
    if (running) singleFrame ! ()
  } withTip ("ESC also starts and stops")

  def setRunning(on: Boolean): Unit = {
    running = on
    run.setSelected(on)
    if (running) singleFrame ! ()
  }

  def refresh(): Unit = {
    display.draw(syncWait = false)
  }

  val radius   = radioButton("Radius", true) { state => }
  val density  = radioButton("Density", false) { state => }
  val speed    = radioButton("Speed", false) { state => }
  val feature  = buttonGroup(radius, density, speed)
  val overruns = textField("", 10){_=>}.withTitledBorder("Average overrun μs").withTip("Average frame overrun time since the last FPS adjustment")
  val reports  = textField("", 60){_=>}.withTitledBorder("Reports")

  val fps = spinner(FPS, 0, 600, 10) {
    value =>
        FPS       = value + 1
        overRun   = 0.0
        overCount = 0
        overruns.setText("None")
  } withTitledBorder ("FPS") withTip ("Target number of frames per (real) second")

  /** Accumulated overrun in nanoseconds since the last FPS change */
  var overRun:   Double = 0.0
  var overCount: Int    = 0

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
    run,
    hGlue,
    // label(s"${allBodies.length}"),
    col(radius, density, speed),
    hGlue,
    fps,
    hGlue,
    wBounce,
    bBounce,
    gravity,
    repel,
    mx,
    time,
    lightspeed,
    hGlue
  ).withEtchedBorder()

  val display = new Display[Body](
    allBodies,
    title,
    width = width,
    height = height,
    events = fromDisplay,
    keys = fromDisplay,
    components = fromDisplay,
    motions = fromDisplay,
    North = controls,
    South = row(reports, hGlue, overruns)
  )

}
