import io.threadcso._

/** Driver for Boids simulations.
  *
  * Simplified interaction with `Boid` characteristics uses the keyboard. This
  * could be refined to incorporate a GUI, but life's too short.
  */
object Boids {
  def main(args: Array[String]) = {
    var N = 250
    var debugPort = 0
    import scala.math.Pi
    import Boid._
    import BBox._

    def setParam(arg: String): Unit = {
      if (arg.matches("[0-9]+")) N = arg.toInt
      else if (arg.matches("[0-9]+x[0-9]+")) {
        val x = arg.indexOf('x')
        xSize = arg.substring(0, x).toInt
        ySize = arg.substring(x + 1).toInt
      } else if (arg == "S+") BoidDisplay.Synchronizing(true)
      else if (arg == "S-") BoidDisplay.Synchronizing(false)
      else if (arg == "A-") BoidDisplay.antiAlias = false
      else if (arg == "A+") BoidDisplay.antiAlias = true
      else if (arg.startsWith("R=")) Rate = arg.substring(2).toInt
      else if (arg.startsWith("drag=")) drag = arg.substring(5).toDouble
      else if (arg.startsWith("co=")) Coh = arg.substring(3).toDouble
      else if (arg.startsWith("se=")) Sep = arg.substring(3).toDouble
      else if (arg.startsWith("al=")) Align = arg.substring(3).toDouble
      else if (arg.startsWith("r=")) range = arg.substring(2).toDouble
      else if (arg.startsWith("V=")) maxSpeed = arg.substring(2).toDouble
      else if (arg.startsWith("v=")) minSpeed = arg.substring(2).toDouble
      else if (arg.startsWith("f=")) breadth = Pi * arg.substring(2).toDouble
      else if (arg.startsWith("-d")) debugPort = arg.substring(2).toInt
      else
        Console.println(f"""
          S${if (BoidDisplay.Synchronizing) "+"
          else "-"}\t\tsynchronising (+) or not (-)
          A${if (BoidDisplay.antiAlias) "+"
          else "-"}\t\tantialiasing (+) or not (-)
          R=$Rate\t\tframe rate (ms/frame)
          drag=$drag\tdrag coefficient 
          co=$Coh\tCohesiveness
          se=$Sep\tMin separation
          al=$Align\tTendency to align with visible neighbours
          f=${breadth / Pi}%1.4g*π\tField of vision 
          r=$range\tRange of vision
          V=$maxSpeed\tMax velocity
          v=$minSpeed\t\tMin velocity
         """)
      prompt()
    }

    def prompt(): Unit = {
      Console.print("> ")
      Console.flush()
    }

    def printParams(): Unit = {
      val syncing = if (BoidDisplay.Synchronizing) '+' else '-'
      val antialiasing = if (BoidDisplay.antiAlias) '+' else '-'
      Console.println(
        "%d %dx%d R=%d S%c A%c drag=%4.3g co=%4.3g se=%4.3g al=%4.3g f=%4.3g r=%04.3g V=%4.3g v=%4.3g"
          .format(
            N,
            xSize,
            ySize,
            Rate,
            syncing,
            antialiasing,
            drag,
            Coh,
            Sep,
            Align,
            breadth / Pi,
            range,
            maxSpeed,
            minSpeed
          )
      )
      prompt()
    }

    val kbd = OneOne[String]("Keyboard")

    val interaction = proc("Interaction") {
      printParams()

      serve(
        kbd =?=> { cmd =>
          {
            for (arg <- cmd.split("[ \t]+")) setParam(arg)
            printParams()
          }
        }
      )
    }

    for (arg <- args) setParam(arg)

    if (debugPort != 0) {
      sys.props.put("io.threadcso.debugger.port", s"$debugPort")
      println(debugger)
    }

    (component.keyboard(kbd) || interaction).fork
    run(BoidSystem(N))
  }
}
