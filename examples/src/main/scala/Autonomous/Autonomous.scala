import app.OPT._
import display._
import io.threadcso._

import java.awt.Color

/**
  *  Atonomous bodies
  */
object Autonomous extends App {

  ////
  val Command = "Autonomous"

  val allBodies = new collection.mutable.Queue[Body]

  val Options = List(

  )

  type Position = Vector.Variable
  type Velocity = Vector.Variable
  type Force = Vector.Value
  type ForceVariable = Vector.Variable

  var deltaT: Double = 1.0
  var wallBounce, ballBounce: Double = 1.0
  var width: Int = 800
  var height: Int = 700
  var scale = -11
  var G = scaledG() // Gravitational constant, dimensions are m^3/k/s^2

  def scaledG() = 6.79 * math.pow(10.0, scale)

  var FPS: Int = 25 // Frames/second
  /** Maximum speed */
  var CF = 16.0 // Fudge-factor for calculating max particle speed
  var C: Double = Vector.Value(width / (CF * deltaT), height / (CF * deltaT), 0.0).magnitude

  /** Particle representation
    *
    * @param R
    * fixed radius
    * @param position
    * current position
    * @param velocity
    * current velocity
    */
  class Body(
              var R: Double,
              val position: Position,
              val velocity: Velocity = new Velocity()
            ) extends Displayable {
    self =>
    override def toString: String = s"Particle($R, $position, $velocity)"

    private[this] var density = 10.0

    private[this] def vol = (4.0 / 3.0) * math.Pi * (R * R * R)

    private[this] var volume = vol

    def setR(radius: Double): Unit = {
      R = radius;
      volume = vol
    }

    /** Current mass of the particle */
    def mass = density * volume

    /** Calculate the position and velocity after time deltaT given the total
      * force on the particle
      */
    def nextState(totalForce: Force): Unit = {
      position += velocity * deltaT
      // Bounce off the edges and lose some momentum if necessary
      if (position.x < R + 10 && velocity.x < 0) {
        velocity.scaleX(-wallBounce);
        position.x = R + 10
      }
      if (position.x + R + 10 > width && velocity.x > 0) {
        velocity.scaleX(-wallBounce);
        position.x = width - R - 10
      }
      if (position.y < R + 10 && velocity.y < 0) {
        velocity.scaleY(-wallBounce);
        position.y = R + 10
      }
      if (position.y + R + 10 > height && velocity.y > 0) {
        velocity.scaleY(-wallBounce);
        position.y = height - R - 10
      }
      // limit speed
      val nextV = velocity + totalForce * deltaT / mass
      if (nextV.magnitude < C) velocity := nextV
    }

    /** Calculate the force attraction on this particle by that particle */
    def attractionTo(that: Body): Force = {
      val magnitude =
        G * this.mass * that.mass / (this.position squareTo that.position)
      (this.position directionTo that.position) * magnitude
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


    val instructions: Chan[Message] = OneOne[Message]

    val controller: PROC = proc("Body") {
      val others = new collection.mutable.Queue[Body]
      repeat {
        instructions ? {
          case Step =>
            val localForce = new ForceVariable()
            // calculate forces between this and the others
            for (other <- others if other ne self) {
              val force = (self attractionTo other) * (if (self touches other) ballBounce else 1.0)
              localForce += force
            }
            self.nextState(localForce)
            ()

          case AddBody(body) =>
            others += body
            ()
        }
      }
    }
  }

  trait Message

  case object Step extends Message

  case class AddBody(body: Body) extends Message

  case class RemoveBody(body: Body) extends Message

    /** Whether the simulation is running or not */
  var running = false

    /** Process that reacts to mouse and keyboard events on the display
      */
  val mouseManager = proc("mouseManager") {
      repeat {
        val e = GUI.fromDisplay ? {
          case Pressed(mouse) => {
            val lastHits = GUI.display.at(mouse.x, mouse.y, running)

            if (!running && lastHits.length == 1)
              for (body <- lastHits)
                println(s"R=${body.R}, mass=${body.mass}")

            if (mouse.isShift && !running) {
              // We are selecting particles
              for (body <- lastHits) body.selected = !body.selected
              GUI.display.draw(syncWait = false)

            } else { // remove selection (if selected) or change density
              val factor = if (mouse.isControl) 0.5 else 2.0
              for (body <- lastHits)
                if (body.selected)
                  body.selected = false
                else
                  body.changeDensity(factor)
              // regenerate the display if necessary
              if (!running) GUI.display.draw(syncWait = false)
            }
          }

          // case Entered(mouse) => if (!running) display.draw()

          case KeyPressed(key) =>
            // nudge selected particles with arrows
            // shrink selected particles with 1 (all particles if control)
            // grow selected particles with 2 (all particles if control)
            var changed = true
            if (!running) for (body <- allBodies) {
              import java.awt.event.KeyEvent.{VK_DOWN, VK_LEFT, VK_RIGHT, VK_UP}
              key.code match {
                case VK_LEFT =>
                  if (body.selected)
                    body.position.x = body.position.x - 5
                case VK_UP =>
                  if (body.selected)
                    body.position.y = body.position.y - 5
                case VK_RIGHT =>
                  if (body.selected)
                    body.position.x = body.position.x + 5
                case VK_DOWN =>
                  if (body.selected)
                    body.position.y = body.position.y + 5
                case '1' =>
                  if (body.selected)
                    body.setR(body.R * 2.0 / 3.0)
                case '2' =>
                  if (body.selected)
                    body.setR(body.R * 3.0 / 2.0)
                case _ =>
                  changed = false
              }
            }
            if (changed) GUI.display.draw(syncWait = false)
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


    object GUI {

      import widgets._

      val fromDisplay = N2NBuf[Event[Body]](50, writers = 0, readers = 0)

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

      val time = spinner(deltaT, 0.1, 50.0, 0.25) { value =>
        deltaT = value
      } withTitledBorder ("∂T")

      val fps = spinner(FPS, 1, 60, 10) { value =>
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
        label(s"${allBodies.length}"),
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

      val display = new Display[Body](
        allBodies,
        "Bodies",
        width = width,
        height = height,
        events = fromDisplay,
        keys = fromDisplay,
        North = controls
      )
    }

    def Main(): Unit = {
      println(debugger)
      G = scaledG()

      // Seed the world
      if (allBodies.isEmpty) {
        allBodies += new Body(60, new Position(width * 0.5, height * 0.66), new Velocity(-1, +1, 0))
        allBodies += new Body(90, new Position(width * 0.5, height * 0.33), new Velocity(-1, +1, 0))
      }

      val workers: PROC = ||(for {wid <- 0 until allBodies.length} yield allBodies(wid).controller)

      val displayController: PROC = proc("DisplayController") {
        // set up the bodies
        for {body <- allBodies} {
          for {other <- allBodies if other ne body} {
            body.instructions ! AddBody(other)
          }
        }

        repeat {
          takeTime(seconds(1.0 / FPS)) {
            GUI.display.draw()
            run(||(for {body <- allBodies} yield proc {
              body.instructions ! Step
            }))
          }
        }
      }



      // run the workers, mousemanager, and display concurrently
      (workers || displayController || mouseManager)()
    }
}
