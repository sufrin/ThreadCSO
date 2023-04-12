import Message._
import Types._
import app.OPT._
import display._
import io.threadcso._

/**
  *
  *  This little, almost pointless, example was intended to explore the practicality of working
  *  with autonomous interacting bodies that have different characteristics, when
  *  their interactions are governed by physical as well as logical laws.
  *
  *  The framework is flexible, but the present example has only a couple of kinds of
  *  body, namely "Immobiles" which have mass but can't move, and "Spheres" which
  *  have mass and can move.
  *
  *  In contrast with the `Gravitation` example, which exemplifies barrier-mediated
  *  concurrency in which all objects (including the display) are controlled by
  *  workers that run in synchrony, this example associates each body with its own
  *  "internal" controller that computes its new state on receiving `Tick` messages.
  *
  */
object Autonomous extends App {

  val Command = "Particles"

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

  var NumberOfSpheres: Int = 0
  var NumberOfImmobiles: Int = 0

  val Options = List(
    OPT("s", NumberOfSpheres,   "«int» number of mobile spheres"),
    OPT("i", NumberOfImmobiles, "«int» number of immobiles"),
  )


  object GUI extends UserInterface(allBodies)

  @inline private def running: Boolean = GUI.running
    /**
      *   Process that reacts to all display events
      */
  val interactionManager = proc("interactionManager") {
      var mouseX, mouseY, lastX, lastY: Double = -1.0
      var down = false

      repeat {
        GUI.fromDisplay ? {

          case ComponentResized(detail) =>
            GUI.width  = Display.toPixels(detail.w)
            GUI.height = Display.toPixels(detail.h)
            GUI.display.setDimensions(GUI.width, GUI.height)

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
                   launchBody(new Sphere(20, new Position(mouseX, mouseY), new Velocity(0, 0, 0), GUI))
              case VK_Z | VK_X if !running =>
                   launchBody(new Immobile(20, new Position(mouseX, mouseY), new Velocity(0, 0, 0), GUI))
              case VK_SPACE  =>
                   GUI.setRunning(!running)
                   if (running) GUI.report(s"${allBodies.length} bodies")
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
        GUI.reportOverrun(-ahead.toDouble)
      else
        sleep(ahead)
    }



    def Main(): Unit = {
      println(debugger)

      // Seed the world
      if (allBodies.isEmpty) {
        val NumberOfBodies = NumberOfImmobiles + NumberOfImmobiles
        for { i<-0 until NumberOfSpheres }
          allBodies += new Sphere(10 + (2*allBodies.length min (GUI.width / NumberOfBodies)),
            new Position(math.random() * GUI.width, math.random() * GUI.height),
            new Velocity(0, 0, 0), GUI)

        for { i<-0 until NumberOfImmobiles }
          allBodies += new Immobile(10 + (2*allBodies.length min (GUI.width / NumberOfBodies)),
            new Position(math.random() * GUI.width, math.random() * GUI.height),
            new Velocity(0, 0, 0), GUI)
      }


      val displayController: PROC = proc("DisplayController") {

        for {body <- allBodies} body.controller.fork

        repeat {
          takeTime(seconds(1.0 / GUI.FPS)) {
            // draw the display
            GUI.display.draw()
            // advance each body's clock: concurrently
            run(||(for { body <- allBodies } yield proc { body.instructions ! Tick }))
          }
          if (!running) { GUI.singleFrame?() }
        }
      }



      // run the bodies, mousemanager, and display concurrently
      (displayController || interactionManager)()
    }
}
