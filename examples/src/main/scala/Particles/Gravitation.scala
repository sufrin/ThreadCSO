
import Message._
import Types._
import app.OPT._
import display._
import io.threadcso._
import io.threadcso.coerce.UnitProc

/**
  *
  *  Implementing gravitation-like forces can turn out to be rather
  *  inefficient if on every "tick" of the display it is necessary to compute
  *  all N*N inter-body forces. The cost and time can be reduced by noting that the force of
  *  "A" on "B" is the opposite of the force of "B" on "A". This is an example
  *  of how such a reduction is implemented. The details are explained
  *  in the lecture notes for particle computations.
  *
  */
object Gravitation extends App {

  val Command = "Gravitation"

  val allBodies = new collection.mutable.Queue[Body]

  /** Number of NumberOfWorkers */
  var NumberOfWorkers:    Int = 4
  /** Half the Quota for each worker to control */
  var Quota: Int = 1
  /** The total number of particles */
  @inline def Particles:  Int = 2 * Quota * NumberOfWorkers

  @inline def forSelectedBodies(effect: Body => Unit): Unit = {
      for { body <- allBodies if body.selected } effect(body)
  }

  @inline def forAllBodies(effect: Body => Unit): Unit = {
    for { body <- allBodies } effect(body)
  }

  val Options = List(
    OPT("w", NumberOfWorkers, "«w: int» number of workers"),
    OPT("q", Quota, "«q: int» each worker controls 2*q bodies"),
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
             case VK_SPACE  =>
                   GUI.setRunning(!running)
                   if (running) GUI.report(s"$NumberOfWorkers workers; $Particles particles")
                   changed = false
              case VK_A if CONTROL && !running  =>
                    forAllBodies { body => body.selected = true }
              case VK_ESCAPE if !running =>
                    forSelectedBodies(body => body.instructions!DeltaV(0.0))
              case _ =>
            }
            if (changed) GUI.refresh()

          case _ =>
            {}
        }
      }
    }

  /**
    * Evaluate `body`, taking at least `t` nanoseconds. Sleep if evaluation
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
      println(s"$NumberOfWorkers workers, controlling $Particles particles ($Quota/worker)")
      // Seed the particles
      if (allBodies.isEmpty) {
        allBodies += new Sphere(60, new Position(GUI.width * 0.5, GUI.height * 0.66), new Velocity(0, 0, 0), GUI)
        allBodies += new Sphere(90, new Position(GUI.width * 0.5, GUI.height * 0.33), new Velocity(0, 0, 0), GUI)
        while (allBodies.length<Particles)
          allBodies += new Sphere(10 + (allBodies.length min (GUI.width/Particles)),
                                  new Position(math.random()*GUI.width, math.random()*GUI.height),
                                  new Velocity(0, 0, 0), GUI)
      }

    val barrier:   LogBarrier = LogBarrier(NumberOfWorkers+1)
    /** Identity of the Display Process */
    val displayID: Int        = NumberOfWorkers

    val localForces: Array[Array[ForceVariable]] =
        Array.ofDim[Position](NumberOfWorkers, Particles)

    def worker(identity: Int, ownedParticles: Seq[Int]): PROC =
      proc(s"worker($identity)") {
        var ticks, lastMX = 0
        // initialize
        val localForce = localForces(identity)
        for (pid <- 0 until Particles) localForce(pid) = new ForceVariable()
        // println(s"$identity owns $ownedParticles")
        barrier.sync(identity)

        // work
        while (true) { // update local state
          for (pid <- 0 until Particles) localForce(pid).setZero()
          // calculate forces between managed particles
          for (pid <- ownedParticles)
              for (other <- pid + 1 until Particles) {
            val p = allBodies(pid)
            val q = allBodies(other)

            val force =
              (p attractionTo q) * GUI.G * (if (p touches q) GUI.bodyBounce else 1.0)
                ticks += 1
                if ((ticks - lastMX > 10) && GUI.massExchange && (p touches q) && (p.mass < q.mass)) {
                  lastMX = ticks
                  val deltaMass = q.mass * 0.2
                  GUI.report(s"MX: $deltaMass")
                  run(p.instructions!DeltaD(deltaMass / p.vol) || q.instructions!DeltaD(-deltaMass / q.vol))
                }

            localForce(pid)   += force
            localForce(other) -= force
          }
          barrier.sync(identity)

          // update global state of all my particles
          for (pid <- ownedParticles) {
            val force = new ForceVariable()
            for (w <- 0 until NumberOfWorkers) force += localForces(w)(pid)
            allBodies(pid).nextState(force)
          }
          barrier.sync(identity)
        }
      }

    val displayController: PROC = proc("DisplayController") {
        // start the body controllers to respond to keystrokes
        for {body <- allBodies} body.controller.fork
        //
        barrier.sync(displayID)
        repeat {
          takeTime(seconds(1.0 / GUI.FPS)) {
            // draw the entire display
            GUI.display.draw()
            // sync myself
            barrier.sync(displayID)
            barrier.sync(displayID)
          }
          if (!running) { GUI.singleFrame?() }
        }
      }

    /** The particles owned by worker with the given `identity`. */
    def ownedBy(identity: Int): Seq[Int] =
      ((identity * Quota) until ((identity + 1) * Quota)) ++
      (((2 * NumberOfWorkers - identity - 1) * Quota) until ((2 * NumberOfWorkers - identity) * Quota))

    val allWorkers = ||(for { identity <- 0 until NumberOfWorkers  } yield worker(identity, ownedBy(identity)))

    // run the bodies, mousemanager, and display concurrently
    (displayController || interactionManager || allWorkers)()
  }
}
