import app.OPT._
import display._
import io.threadcso._

import java.awt.Color

/** A demonstration of pseudo-gravitational particle calculations done by
  * several workers in lock-step and coordinated by barriers. The demonstration
  * can also show the effects of confinement of gas particles in a closed
  * chamber whose walls add energy to particles that collide with them
  *
  *   1. the particles are in a closed container with energy absorbent walls
  *
  * 2. there is an upper bound on the speed to which they can accelerate
  *
  * A description of particle computations is given in the lecture notes.
  */
object Particles extends App {
  var deltaT = 3.0 // Time quantum for simulation
  var scale = -11 // G = 6.79Escale
  var G = scaledG() // Gravitational constant, dimensions are m^3/k/s^2
  var ballBounce = 1.0 // Coefficient of wallBounce between touching particles
  var wallBounce = 0.9 // Coefficient of wallBounce at walls
  var CF = 16.0 // Fudge-factor for calculating max particle speed
  ////
  var P = 4 // Number of worker processes
  var Seg =
    1 // Each worker controls (does the calculations for) `2*Seg` particles
  var N = 2 * P * Seg // Number of particles
  var FPS = 600 // Frames/second
  ////
  var width = 800
  var height = 700
  var debug = false
  ////
  val Command = "Particles"

  val ps = new collection.mutable.Queue[Particle]

  def scaledG() = 6.79 * math.pow(10.0, scale)

  val Options = List(
    OPT("S", Seg, s"«S: int» ($Seg) each worker manages 2S particles "),
    OPT("P", P, s"«P: int» ($P) number of worker processes "),
    OPT("t", deltaT, s"«∂t: double» ($deltaT) time quantum for simulation"),
    OPT(
      "s",
      scale,
      s"«scale: int» ($scale) order of magnitude of 'gravitational' constant: G = 6.79E<scale>"
    ),
    OPT(
      "W",
      wallBounce,
      s"«Wall: double» ($wallBounce) When it hits a wall a particle's momentum *= -Wall."
    ),
    OPT(
      "B",
      ballBounce,
      s"«Ball: double» ($ballBounce) force multiplier touching particles [negative => repulsion]"
    ),
    OPT(
      "FPS",
      FPS,
      s"«FPS: int» ($FPS) frames to be shown per second (target value)"
    ),
    OPT("w=", width, s"«int» ($width) width of the arena (units)"),
    OPT("h=", height, s"«int» ($height) height of the arena (units)"),
    OPT(
      "C",
      CF,
      s"«CF: double» ($CF) particle speed is limited to |(width/CF∂t, height/CF∂t)|."
    ),
    OPT("-d", debug, s"enable the debugger ($debug)"),
    REST(
      "--",
      queueSpecs(_),
      "Remaining parameters are particle specs of the form <radius>@<x>,<y>"
    )
  )

  type Position = Vector.Variable
  type Velocity = Vector.Variable
  type Force = Vector.Value
  type ForceVariable = Vector.Variable

  def queueSpecs(specs: List[String]): Unit = {
    for (s <- specs) try { ps.enqueue(mkPart(s)) }
    catch { case e: Exception => println(s"$s is not a valid particle spec") }
  }

  def mkPart(s: String): Particle = {
    val parts = s.split("[@,]")
    new Particle(
      parts(0).toDouble,
      new Position(parts(1).toDouble, parts(2).toDouble)
    )
  }

  /** The indices of the particles owned by process `me` */
  def ownedBy(me: Int): Seq[Int] =
    ((me * Seg) until ((me + 1) * Seg)) ++ (((2 * P - me - 1) * Seg) until ((2 * P - me) * Seg))

  /** Arbitrary upper bound on present speed of a particle that is to be
    * accelerated: calculated at startup
    */
  var C: Double = _
  val Origin: Vector.Value = Vector.Value(0, 0)

  /** Bottom right corner of the container: calculated at startup */
  var Corner: Vector.Value = _

  /** Particle representation
    *
    * @param R
    *   fixed radius
    * @param position
    *   current position
    * @param velocity
    *   current velocity
    */
  class Particle(
      var R: Double,
      val position: Position,
      val velocity: Velocity = new Velocity()
  ) extends Displayable {
    override def toString: String = s"Particle($R, $position, $velocity)"

    private[this] var density = 10.0
    private[this] def vol = (4.0 / 3.0) * math.Pi * (R * R * R)
    private[this] var volume = vol

    def setR(radius: Double): Unit = { R = radius; volume = vol }

    /** Current mass of the particle */
    def mass = density * volume

    /** Calculate the position and velocity after time deltaT given the total
      * force on the particle
      */
    def nextState(totalForce: Force): Unit = {
      position += velocity * deltaT
      // Bounce off the edges and lose some momentum if necessary
      if (position.x < R + 10 && velocity.x < 0) {
        velocity.scaleX(-wallBounce); position.x = R + 10
      }
      if (position.x + R + 10 > width && velocity.x > 0) {
        velocity.scaleX(-wallBounce); position.x = width - R - 10
      }
      if (position.y < R + 10 && velocity.y < 0) {
        velocity.scaleY(-wallBounce); position.y = R + 10
      }
      if (position.y + R + 10 > height && velocity.y > 0) {
        velocity.scaleY(-wallBounce); position.y = height - R - 10
      }
      // limit speed
      val nextV = velocity + totalForce * deltaT / mass
      if (nextV.magnitude < C) velocity := nextV
    }

    /** Calculate the force attraction on this particle by that particle */
    def attractionTo(that: Particle): Force = {
      val magnitude =
        G * this.mass * that.mass / (this.position squareTo that.position)
      (this.position directionTo that.position) * magnitude
    }

    /** When the particles touch */
    def touches(that: Particle): Boolean =
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

    /** Particle's redness on the display: determined by its density (up to a
      * point)
      */
    def color: Color = {
      import math.{log, max, min}
      val redness = min(0.8, log(max(density, 1.0)) / 10.0)
      val greenness = 0.8 - redness
      val blueness = 0.8 - redness
      new Color(redness.toFloat, greenness.toFloat, blueness.toFloat, 0.7f)
    }

    /** Is the particle selected? */
    var selected: Boolean = false

  }

  /** Barrier for synchronising updates */
  var barrier: Barrier = _

  /** Collection of interacting particles */
  var allParticles: Array[Particle] = _

  /** Per-worker mapping from particles to force */
  var localForces: Array[Array[ForceVariable]] = _

  /** Worker `me` manages the particles in `mine`
    */
  def worker(me: Int, mine: Seq[Int]): PROC =
    proc(s"worker($me)") {
      // initialize
      val localForce = localForces(me)
      for (pid <- 0 until N) localForce(pid) = new ForceVariable()
      barrier.sync()

      // work
      while (true) { // update local state
        for (pid <- 0 until N) localForce(pid).setZero()
        // calculate forces between managed particles
        for (pid <- mine) for (other <- pid + 1 until N) {
          val p = allParticles(pid)
          val q = allParticles(other)

          val force =
            (p attractionTo q) * (if (p touches q) ballBounce else 1.0)
          localForce(pid) += force
          localForce(other) -= force
        }
        barrier.sync()

        // update global state
        for (pid <- mine) {
          val force = new ForceVariable()
          for (w <- 0 until P) force += localForces(w)(pid)
          allParticles(pid).nextState(force)
        }
        barrier.sync()
      }
    }

  /** GUI window */
  var display: Display[Particle] = _

  /** Channel receiving events reported from the GUI. */
  val fromDisplay = N2NBuf[Event[Particle]](50, writers = 0, readers = 0)

  /** Whether the simulation is running or not */
  var running = false

  /** Process that reacts to mouse and keyboard events on the display
    */
  val mouseManager = proc("mouseManager") {
    repeat {
      val e = fromDisplay ? {
        case Pressed(mouse) => {
          val lastHits = display.at(mouse.x, mouse.y, running)

          if (!running && lastHits.length == 1)
            for (particle <- lastHits)
              println(s"R=${particle.R}, mass=${particle.mass}")

          if (mouse.isShift && !running) {
            // We are selecting particles
            for (particle <- lastHits) particle.selected = !particle.selected
            display.draw(syncWait = false)
          } else { // remove selection (if selected) or change density
            val factor = if (mouse.isControl) 0.5 else 2.0
            for (particle <- lastHits)
              if (particle.selected)
                particle.selected = false
              else
                particle.changeDensity(factor)
            // regenerate the display if necessary
            if (!running) display.draw(syncWait = false)
          }
        }

        // case Entered(mouse) => if (!running) display.draw()

        case KeyPressed(key) =>
          // nudge selected particles with arrows
          // shrink selected particles with 1 (all particles if control)
          // grow selected particles with 2 (all particles if control)
          if (!running) for (particle <- allParticles) {
            import java.awt.event.KeyEvent.{VK_DOWN, VK_LEFT, VK_RIGHT, VK_UP}
            key.code match {
              case VK_LEFT =>
                if (particle.selected)
                  particle.position.x = particle.position.x - 5
              case VK_UP =>
                if (particle.selected)
                  particle.position.y = particle.position.y - 5
              case VK_RIGHT =>
                if (particle.selected)
                  particle.position.x = particle.position.x + 5
              case VK_DOWN =>
                if (particle.selected)
                  particle.position.y = particle.position.y + 5
              case '1' =>
                if (particle.selected)
                  particle.setR(particle.R * 2.0 / 3.0)
              case '2' =>
                if (particle.selected)
                  particle.setR(particle.R * 3.0 / 2.0)
              case _ =>
            }
          }
          display.draw(syncWait=false)

        case _ => {}
      }
    }
  }

  /** Evaluate `body`, taking at least `t` nanoseconds. Sleep if evaluation
    * terminates early, and report the overrun as a multiple of `t` otherwise.
    */
  def takeTime(t: Nanoseconds)(body: => Unit): Unit = {
    val deadline = nanoTime + t
    body
    val ahead = deadline - nanoTime
    if (ahead <= 0)
      reportOverrun(-ahead.toDouble / t.toDouble)
    else
      sleep(ahead)
  }

  def reportOverrun(behind: Double) = print(f"($behind%1.1f)")

  /** This process sets up the display and GUI, then synchronises the display
    * with the workers.
    */
  val displayController = proc("Display") { // initialize
    import widgets._

    // Number of display cycles to permit when stopping
    var stopping = 0

    /** Permission to draw a single frame */
    val singleFrame = BooleanSemaphore(running)

    var repelling = 1.0
    val gravity = row(
      label("6.79E"),
      spinner(scale, -24, 2, 1) { value =>
        scale = value; G = scaledG() * repelling
      }.alignLeft
    ) withTitledBorder ("Gravity")
    val repel = col(checkBox("  ", false) { state =>
      repelling = if (state) -1.0 else 1.0; G = math.abs(G) * repelling
    }) withTitledBorder ("Repel")

    val wBounce = spinner(wallBounce, -1.0, 5.0, 0.1) { value =>
      wallBounce = value
    } withTitledBorder ("Wall")
    val bBounce = spinner(ballBounce, -50.0, 50.0, 0.1) { value =>
      ballBounce = value
    } withTitledBorder ("Ball")
    val time = spinner(deltaT, 0.1, 50, 0.5) { value =>
      deltaT = value
    } withTitledBorder ("∂T")
    val fps = spinner(FPS, 0, 600, 10) { value =>
      FPS = value + 1
    } withTitledBorder ("FPS")

    // a change in state to true signals the display controller
    val run = checkBox("Run", running) { state =>
      running = state
      if (running) singleFrame.release()
    }

    def controls = row(
      run,
      hGlue,
      label(s"$P workers, $N particles"),
      hGlue,
      fps,
      hGlue,
      wBounce,
      bBounce,
      gravity,
      repel,
      time,
      hGlue
    ).withEtchedBorder()

    display = new Display[Particle](
      allParticles,
      "Particles",
      width = width,
      height = height,
      events = fromDisplay,
      keys = fromDisplay,
      North = controls
    )

    barrier.sync() // wait for the workers' first syncs

    while (true) {

      // drawing the frame overlaps with local updates
      display.draw()

      // wait here for next frame permission
      // so that the last draw is completed when `!running`
      singleFrame.acquire()

      // recompute the new frame
      takeTime(seconds(1.0 / FPS)) {
        barrier.sync() // local updates done
        barrier.sync() // global updates done
      }

      // allow the next cycle to proceed
      if (running)
        singleFrame.release()
    }
  }

  def Main(): Unit = {
    if (debug) println(debugger)
    N = 2 * P * Seg
    C =
      Vector.Value(width / (CF * deltaT), height / (CF * deltaT), 0.0).magnitude
    Corner  = Vector.Value(width, height)
    barrier = new Barrier(P + 1)

    allParticles = Array.ofDim[Particle](N)
    localForces = Array.ofDim[Position](P, N)

    G = scaledG()

    // Seed the world
    if (ps.isEmpty) {
      ps += new Particle(60, new Position(width * 0.5, height * 0.66))
      ps += new Particle(90, new Position(width * 0.5, height * 0.33))
    }
    for (i <- 0 until N)
      allParticles(i) =
        if (ps.isEmpty)
          new Particle(
            (10 + i) min (width / N),
            new Position(math.random() * width, math.random() * height)
          )
        else ps.dequeue()

    // define the workers
    val workers = ||(for (me <- 0 until P) yield worker(me, ownedBy(me)))

    // run the workers, mousemanager, and display concurrently
    (workers || displayController || mouseManager)()
  }

}
