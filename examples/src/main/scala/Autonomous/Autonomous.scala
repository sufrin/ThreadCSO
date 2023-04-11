import app.OPT._
import display._
import io.threadcso._

/**
  *
  *  This little, almost pointless, example was intended to let us explore the practicality of working
  *  with autonomous interacting bodies that have different characteristics, when
  *  their interactions are governed by physical as well as logical laws.
  *
  *  The framework is flexible, but the present example has only a couple of kinds of
  *  body, namely "Immobiles" which have mass but can't move, and "Spheres" which
  *  have mass and can move.
  *
  *  In contrast with the `Particles` example, which exemplifies barrier-mediated
  *  concurrency in which all objects (including the display) are controlled by
  *  workers that run in synchrony, this example associates each body with its own
  *  "internal" controller.
  *
  *  Implementing gravitation-like forces can turn out to be rather
  *  inefficient if on every "tick" of the display it is necessary to compute
  *  all N*N inter-body forces. The cost can be reduced by noting that the force of
  *  "A" on "B" is the opposite of the force of "B" on "A". The detail is left as an
  *  exercise.
  *
  *
  */
object Autonomous extends App {

  ////
  val Command = "Autonomous"

  val allBodies = new collection.mutable.Queue[Body]

  def deleteBody(other: Body): Unit = {
      other.instructions.closeOut()
      run(||(for { body <- allBodies if body != other } yield proc { body.instructions!RemoveBody(other) }))
      allBodies -= other
  }

  @inline def forSelectedBodies(effect: Body => Unit): Unit = {
      for { body <- allBodies if body.selected } effect(body)
  }

  @inline def forAllBodies(effect: Body => Unit): Unit = {
    for { body <- allBodies } effect(body)
  }

  val Options = List(

  )

  type Position = Vector.Variable
  type Velocity = Vector.Variable
  type Force = Vector.Value
  type ForceVariable = Vector.Variable

  var deltaT:     Double  = 1.0
  var bodyBounce: Double  = -0.9
  var wallBounce: Double  = 0.9
  var width:      Int     = 1200
  var height:     Int     = 800
  var scale:      Int     = -7
  def scaleG:     Double  = 6.79 * math.pow(10.0, scale)
  var G:          Double  = scaleG // Gravitational constant, dimensions are m^3/k/s^2

  var FPS: Int = 50 // Frames/second
  /** Maximum speed */

  var C: Double = 30.0

  trait       Message
  case object Tick                    extends Message
  case class  AddBody(body: Body)     extends Message
  case class  RemoveBody(body: Body)  extends Message
  case class  DeltaR(delta: Double)   extends Message
  case class  DeltaD(delta: Double)   extends Message
  case class  DeltaV(delta: Double)   extends Message

    /** Whether the simulation is running or not */
  var running = false

    /**
      *   Process that reacts to all display events
      */
  val interactionManager = proc("interactionManager") {
      var mouseX, mouseY, lastX, lastY: Double = -1.0
      var down = false

      repeat {
        val e = GUI.fromDisplay ? ()
        e match {

          case ComponentResized(detail) =>
            width  = Display.toPixels(detail.w)
            height = Display.toPixels(detail.h)
            GUI.display.setDimensions(width, height)

          case Dragged(mouse) if !running =>
               val dx = mouse.x - lastX
               val dy = mouse.y - lastY
               forSelectedBodies {
                  body =>
                    body.position.x = body.position.x + dx
                    body.position.y = body.position.y + dy
               }
               lastX = mouse.x
               lastY = mouse.y
               GUI.refresh()

          case Moved(mouse) if !running =>
               mouseX = mouse.x
               mouseY = mouse.y

          case Released(mouse) if !running =>
               down = false

          case Pressed(mouse) if !running =>
            lastX = mouse.x
            lastY = mouse.y
            down  = true
            val lastHits = GUI.display.at(lastX, lastY, false)

            // add to / remove from the selection
            if (mouse.isControl || mouse.isMeta)
               for (body <- lastHits) {
                 body.selected = !body.selected
                 GUI.report(s"$body")
               }

            // clear or start the selection
            if (!(mouse.isControl || mouse.isMeta)) {
              if (lastHits.isEmpty) {
                for { body <- allBodies } body.selected = false
              } else
              for (body <- lastHits)  {
                body.selected = true
                GUI.report(s"$body")
              }
            }
            GUI.refresh()

          // change the radius or the density or speed of selected bodies
          case Wheel(wheel) if !running =>
            forSelectedBodies {
             body =>
               if (GUI.radius.isSelected) body.instructions!DeltaR(wheel.rotation) // body.R = body.R + wheel.rotation

               if (GUI.density.isSelected) {
                 body.instructions!DeltaD(100*wheel.rotation) // body.density = body.density + factor
               }
               if (GUI.speed.isSelected) {
                val factor = if (wheel.isControl) 0.0 else if (wheel.rotation > 0) 1.5 else 0.6
                body.instructions!DeltaV(factor)
               }
               GUI.report(s"$body")
            }
            GUI.refresh()

          case KeyPressed(key) =>
            var changed = true
            val CONTROL = key.isControl || key.isMeta
            import java.awt.event.KeyEvent._
            key.code match {
              case VK_S | VK_A if !running && !CONTROL =>
                   launchBody(new Sphere(20, new Position(mouseX, mouseY), new Velocity(0, 0, 0)))
              case VK_Z | VK_X if !running =>
                   launchBody(new Immobile(20, new Position(mouseX, mouseY), new Velocity(0, 0, 0)))
              case VK_SPACE  =>
                   GUI.setRunning(!running)
                   changed = false
              case VK_A if CONTROL && !running  =>
                    forAllBodies { body => body.selected = true }
              case VK_ESCAPE if !running =>
                    forSelectedBodies(body => body.instructions!DeltaV(0.0))
              case VK_DELETE | VK_BACK_SPACE if !running  =>
                    // delete the selected bodies
                    val deletedBodies = new collection.mutable.Queue[Body]()
                    forSelectedBodies { body => deletedBodies.enqueue(body) }
                    for { body <- deletedBodies } deleteBody(body)
              case _ =>
            }
            if (changed) GUI.refresh()

          case _ =>
            {}
        }
      }
    }

    /**
      *   Launch the body
      */
    def launchBody(body: Body): Unit =  {
        GUI.report(s"new $body")
        allBodies += body
        body.selected = true
        body.controller.fork
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
      val fromDisplay = N2NBuf[Event[Body]](0, writers = 0, readers = 0)

      /** Permission to draw a single frame */
      val singleFrame = io.threadcso.lock.primitive.UnitChan() // BooleanSemaphore(running)

      var repelling = 1.0

      var massExchange: Boolean = true

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
      } withTitledBorder ("Wall") withTip("Coefficient of restitution of the wall")

      val bBounce = spinner(bodyBounce, -50.0, 50.0, 0.1) { value =>
        bodyBounce = value
      } withTitledBorder ("Bounce") withTip ("Bounciness of other bodies")

      val time = spinner(deltaT, 0.0, 50.0, 0.25) { value =>
        deltaT = value
      } withTitledBorder ("∂T") withTip ("Simulated time increment")

      val fps = spinner(FPS, 0, 600, 20) { value =>
        FPS = value + 1
      } withTitledBorder ("FPS") withTip("Target number of frames per (real) second")

      val lightspeed = spinner(C, 10.0, 200.0, 5) { value =>
        C = value
      } withTitledBorder ("C") withTip ("Maximum simulated velocity")


      // a change in state to true signals the display controller
      val run = checkBox("Running", running) { state =>
        running = state
        if (running) singleFrame!()
      } withTip ("ESC also starts and stops")

      def setRunning(on: Boolean): Unit = {
        running = on
        run.setSelected(on)
        if (running) singleFrame!()
      }
      
      def refresh(): Unit = {
        display.draw(syncWait = false)
      }

      val radius  = radioButton("Radius", true) { state => }
      val density = radioButton("Density", false) { state => }
      val speed   = radioButton("Speed", false) { state => }
      val feature = buttonGroup(radius, density, speed)



      val overruns = label(" "*60)
      val reports = label(" "*60)

      def reportOverrun(behind: Double): Unit = {
        val text = overruns.getText + f"$behind%1.1f "
        overruns.setText(if (text.length < 50) text else f"** $behind%1.1f ")
      }

      def report(message: String): Unit = {
        reports.setText(message)
      }

      def controls = row(
        run,
        hGlue,
        // label(s"${allBodies.length}"),
        col(radius,density, speed),
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
        "Bodies",
        width = width,
        height = height,
        events = fromDisplay,
        keys = fromDisplay,
        components = fromDisplay,
        motions = fromDisplay,
        North = controls,
        South = row(reports withTitledBorder "Reports", hGlue, overruns withTitledBorder "Overruns")
      )
    }

    def Main(): Unit = {
      println(debugger)
      G = scaleG

      // Seed the world
      if (allBodies.isEmpty) {
        //allBodies += new Sphere(90, new Position(width * 0.5, height * 0.33), new Velocity(-1, +1, 0))
      }


      val displayController: PROC = proc("DisplayController") {
        // set up the bodies
        for {body <- allBodies} {
          for {other <- allBodies if other ne body} {
            body.instructions ! AddBody(other)
          }
        }

        for {body <- allBodies} body.controller.fork

        repeat {
          takeTime(seconds(1.0 / FPS)) {
            // draw the display
            GUI.display.draw()
            // advance each body's clock: concurrently
            run(||(for { body <- allBodies } yield proc {
              body.instructions ! Tick
            }))
          }
          if (!running) {

            GUI.singleFrame?()
          }
        }

      }



      // run the bodies, mousemanager, and display concurrently
      (displayController || interactionManager)()
    }
}
