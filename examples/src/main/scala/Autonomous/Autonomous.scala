import app.OPT._
import display._
import io.threadcso._

/**
  *  Autonomous bodies
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
  var width: Int = 1200
  var height: Int = 800
  var scale = -9
  var G = scaledG() // Gravitational constant, dimensions are m^3/k/s^2
  def scaledG() = 6.79 * math.pow(10.0, scale)

  var FPS: Int = 50 // Frames/second
  /** Maximum speed */

  var CF = 10.0 // Fudge-factor for calculating max particle speed
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



  trait       Message
  case object Tick extends Message
  case class  AddBody(body: Body) extends Message
  case class  RemoveBody(body: Body) extends Message

    /** Whether the simulation is running or not */
  var running = false

    /** Process that reacts to mouse and keyboard events on the display
      */
  val mouseManager = proc("mouseManager") {
      repeat {
        val e = GUI.fromDisplay ? {
          case Pressed(mouse) => {
            val lastHits = GUI.display.at(mouse.x, mouse.y, running)

            if (lastHits.length == 1)
              for (body <- lastHits)
                println(s"R=${body.R}, mass=${body.mass}")

            if (mouse.isShift) {
              // We are selecting particles
              for (body <- lastHits) body.selected = !body.selected
              GUI.display.draw(syncWait = false)
            }

              val factor = if (mouse.isControl) 0.5 else 2.0
              for (body <- lastHits)
                if (body.selected)
                  body.selected = false
                else
                  body.changeDensity(factor)
              // regenerate the display if necessary
              if (!running) GUI.display.draw(syncWait = false)
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
        GUI.reportOverrun(-ahead.toDouble / t.toDouble)
      else
        sleep(ahead)
    }


    object GUI {

      import widgets._

      /** Messages arriving from bodies */
      val fromDisplay = N2NBuf[Event[Body]](50, writers = 0, readers = 0)

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

      val wBounce = spinner(wallBounce, -5.0, 5.0, 0.1) { value =>
        wallBounce = value
      } withTitledBorder ("Wall")

      val bBounce = spinner(ballBounce, -50.0, 50.0, 0.1) { value =>
        ballBounce = value
      } withTitledBorder ("Ball")

      val time = spinner(deltaT, 0.0, 50.0, 0.25) { value =>
        deltaT = value
      } withTitledBorder ("∂T")

      val fps = spinner(FPS, 0, 600, 20) { value =>
        FPS = value + 1
      } withTitledBorder ("FPS")

      // a change in state to true signals the display controller
      val run = checkBox("Run", running) { state =>
        running = state
        if (running) singleFrame.release()
      }

      val reports = label("")

      def reportOverrun(behind: Double): Unit = {
        val text = reports.getText + f"$behind%1.1f "
        reports.setText(if (text.length<50) text else f"** $behind%1.1f " )
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
        North = controls,
        South = reports withTitledBorder "Overruns"
      )
    }

    def Main(): Unit = {
      println(debugger)
      G = scaledG()

      // Seed the world
      if (allBodies.isEmpty) {
        allBodies += new Body(60, new Position(width * 0.5, height * 0.66), new Velocity(-1, +1, 0))
        allBodies += new Body(90, new Position(width * 0.5, height * 0.33), new Velocity(-1, +1, 0))
        allBodies += new Body(30, new Position(width * 0.75, height * 0.25), new Velocity(-1, +1, 0))
        allBodies += new Body(20, new Position(width * 0.75, height * 0.75), new Velocity(+1, +1, 0))
      }

      val bodies: PROC = ||(for {wid <- 0 until allBodies.length} yield allBodies(wid).controller)

      val displayController: PROC = proc("DisplayController") {
        // set up the bodies
        for {body <- allBodies} {
          for {other <- allBodies if other ne body} {
            body.instructions ! AddBody(other)
          }
        }

        repeat {
          takeTime(seconds(1.0 / FPS)) {
            // draw the display
            GUI.display.draw()
            // advance each body's clock: concurrently
            run(||(for { body <- allBodies } yield proc {
              body.instructions ! Tick
            }))
          }
          if (!running) GUI.singleFrame.acquire()
        }

      }



      // run the bodies, mousemanager, and display concurrently
      (bodies || displayController || mouseManager)()
    }
}
