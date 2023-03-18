import io.threadcso._
import io.threadcso.lock.LogBarrier

/** `boidSystem(N)` yields the concurrent composition of `N` boid controllers
  * and an associated display with its controller
  */
object BoidSystem {

  def apply(N: Int): PROC = {

    val allBoids = collection.mutable.Set[Boid]()

    val random = new scala.util.Random();

    // Barrier for synchronisation of N Boids and a display controller
    val barrier = LogBarrier(N + 1)

    // Each round contains two barrier synchronisations.
    // After the first synchronisation, but before the second,
    // Boids may read each others' states.
    // After the second synchronisation, Boids may update their own states.

    // Boid #me controller process
    def boidController(me: Int) = proc(s"Bird($me)") {
      // Set up this boid's initial state
      val myBoid = new Boid
      myBoid.init(random)
      allBoids += myBoid

      // Main loop
      while (true) {
        barrier.sync(me) // enter reading phase
        val state = myBoid.newState(allBoids)
        barrier.sync(me) // enter writing phase
        myBoid.setState(state)
      }
    }

    // Display controller process
    val displayController = proc("DisplayController") {
      val me = N
      barrier.sync(me) // wait for Boids to be initialised
      val display = new BoidDisplay(allBoids)
      while (true) {
        barrier.sync(me) // reading
        display.draw()
        sleep(Boid.Rate * milliSec)
        barrier.sync(me) // writing
      }
    }

    val controllers = ||(for (i <- 0 until N) yield boidController(i))

    controllers || displayController
  }

}
