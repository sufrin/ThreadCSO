



import app.OPT._
import display._
import io.threadcso._

/**
  *
  *  Life implementation in which the cells exchange states along channels
  *
  */
object Lyfe extends App {

  val Command = "Lyfe"
  type Cell = LyfeCell

  val allCells = new collection.mutable.Queue[Cell] // TODO: make this an array; it never changes

  @inline def forSelectedCells(effect: Cell => Unit): Unit = {
      for { cell <- allCells if cell.selected } effect(cell)
  }

  @inline def forAllCells(effect: Cell => Unit): Unit = {
    for { cell <- allCells } effect(cell)
  }

  var _cols  = 50
  var _rows  = 40
  var width  = 1000
  var height = 800

  val Options = List(
    OPT("[0-9]+x[0-9]+", { s =>
      val fields = s.split('x')
      width = fields(0).toInt
      height = fields(1).toInt
    }, "width x height in pixels"),
    OPT("[0-9]+X[0-9]+", { s =>
      val fields = s.split('X')
      _cols = fields(0).toInt
      _rows = fields(1).toInt
    }, "cols X rows in cells" ),
    OPT("-h", { Usage() }, "prints usage text"),
  )

  override def Usage(): Unit = {
    super.Usage()
    Console.err.println(
      """
        |The dashboard should be self-explanatory.
        |Cells can be manipulated when the simulation is stopped (and only then).
        |
        |Mouse click      selects the cell the mouse points to.
        |Ctrl-mouseclick  inverts the selection of the cell the mouse points to.
        |
        |mousedrag        selects the cells dragged over
        |ctrl-mousedrag   deselects the cells dragged over
        |
        |Ctrl-Delete/Backspace kills all selected cells.
        |Delete/Backspace      kills all cells.
        |
        |Space brings to life all the selected cells and starts the simulation.
        |Z     brings to life about 1/5th of the cells, randomly chosen.
        |6     switches the birth/survival regime between B3/S23 and B63/S23.
        |
        |""".stripMargin)
  }


  object GUI extends LyfeDashboard(allCells, title="Lyfe", width=width, height=height)

  @inline private def running: Boolean = GUI.running

  var totalCells = 0
  var RADIUS     = 0.0
  var GENERATION = 0

    /**
      *   Process that reacts to all display events
      */
  val interactionManager = proc("interactionManager") {
      var mouseX, mouseY, lastX, lastY: Double = -1.0
      var down = false

      repeat {
        GUI.fromDisplay ? {

          case Dragged(mouse) if !running =>
            val STATE = !(mouse.isControl || mouse.isMeta)
            mouseX = mouse.x
            mouseY = mouse.y
            val (l, r) = ((lastX min mouseX) - RADIUS, lastX max mouseX)
            val (b, t) = ((lastY min mouseY) - RADIUS, lastY max mouseY)
            forAllCells {
              cell =>
                if (l <= cell.x && cell.x <= r && b <= cell.y && cell.y <= t) {
                  cell.selected = STATE
                }
            }
            GUI.refresh()

          case Pressed(mouse) if !running =>
            down  = true
            lastX = mouse.x
            lastY = mouse.y
            val lastHits = GUI.display.at(lastX, lastY, false)

            // add to / remove from the selection
            if (mouse.isControl || mouse.isMeta)
               for (body <- lastHits) {
                 body.selected = !body.selected
               }

            // add to the selection
            if (!(mouse.isControl || mouse.isMeta)) {
                for (cell <- lastHits)  { cell.selected = true }
            }
            GUI.refresh()

          case KeyPressed(key) =>
            var changed = true
            val CONTROL = key.isControl || key.isMeta
            import java.awt.event.KeyEvent._
            key.code match {

              case VK_SPACE  if running =>
                   GUI.setRunning(!running)
                   StatusReport()
                   changed = false

              case VK_SPACE if !running =>
                   forSelectedCells {
                     cell =>
                       cell.selected = false
                       cell.instructions!Live
                    }
                    GUI.setRunning(true)

              case VK_DELETE | VK_BACK_SPACE if !running && !CONTROL =>
                forAllCells {
                  _.state = false
                }
                GENERATION = 0

              case VK_DELETE | VK_BACK_SPACE if !running && CONTROL=>
                forSelectedCells {
                  _.state = false
                }
                GENERATION = 0

              case VK_Z if !running =>
                  val random = new scala.util.Random
                  for (_ <- 0 until totalCells / 5) {
                    val cell = random.nextInt(totalCells)
                    allCells(cell).state = true
                  }

              case VK_6 if (!running) =>
                   forAllCells { cell => cell.b6 = !cell.b6 }

              case _ =>

            }
            if (changed) GUI.refresh()

          case _ =>
            {}
        }
      }
    }

    def StatusReport() = {
      val alive = allCells.filter { _.state }.size
      val longLived = allCells.filter { cell => cell.lifetime >= 3 }.size
      GUI.report(s"Generation: $GENERATION -- $alive / $totalCells living cells of which $longLived are long-lived.")
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

      val neighbours = 8

      val w = GUI.width / _cols
      val h = GUI.height / _rows
      val R = w min h

      val cols   = GUI.width / R
      val rows   = GUI.height / R
      totalCells = rows * cols
      RADIUS     = R

      /**
        *
        * Topologically the playing field is a torus: with westmost
        * and eastmost cells neighbouring, as well as northmost and southmost cells.
        *
        * Intercell channels: each cell has 8 link channels, one to each
        * of its neighbours, with neighbours numbered clockwise from 0 to 7.
        * Neighbour 0 is its "north-west" neighbour; neighbour 1 is its
        * "north" neighbour; ... neighbour 7 is its "west" neighbour.
        */


      val links: Seq[Seq[Seq[Chan[Boolean]]]] =
        for (x <- 0 until cols) yield
          for (y <- 0 until rows) yield
            for (i <- 0 until neighbours) yield
              OneOne[Boolean](s"($x,$y)($i)")

      /**
        * The output links of each cell are indexed by the number of the
        * neighbour to which they send.
        *
        */
      val linkOut: Seq[Seq[Seq[!![Boolean]]]] = links

      /**
        * The input links of each cell are the output
        * links of the corresponding neighbour, to wit:
        * neighbour `i` for output is neighbour `(i+4)%8`
        * for input.
        *
        * The computation of this "view" of the links is
        * straightforward; complicated only by the
        * "wrapping around" of cells on the torus.
        *
        * {{{
        *               out in
        *   north-west  0   4    south-east
        *   north       1   5    south
        *   north-east  2   6    south-west
        *   east        3   7    west
        *               4   0
        *               5   1
        *               6   2
        *   west        7   3    east
        * }}}
        */
      val linkIn: Seq[Seq[Seq[??[Boolean]]]] =
      for (col <- 0 until cols) yield
        for (row <- 0 until rows) yield {
          val west  = (col + cols - 1) % cols
          val east  = (col + 1) % cols
          val north = (row + 1) % rows
          val south = (row + rows - 1) % rows
          val nr = Array(south, south, south, row,  north, north, north, row)
          val nc = Array(east,  col,   west,  west, west,  col,   east,  east)
          for (i <- 0 until neighbours) yield links(nc(i))(nr(i))(i)
        }

      // set up the cells
      for { r <- 0 until rows }
        for  { c <- 0 until cols }
          allCells += new LyfeCell(c*R, r*R, R, R, linkIn(c)(r), linkOut(c)(r))

      // Sends a `Sync` to each cell, concurrently
      val syncCells: PROC = || (for { cell <- allCells } yield π { cell.instructions ! Sync })

      val displayController: PROC = proc("DisplayController") {

          // start the cell controllers
          forAllCells(_.controller.fork)

            repeat {
                takeTime(seconds(1.0 / GUI.FPS)) {
                // draw the display
                GUI.display.draw()
                // synchronize the cells
                syncCells()
              }
              if (GENERATION % 100 == 0) StatusReport()
              GENERATION += 1
              if (!running) { GUI.singleFrame?() }
            }
      }


      // run the bodies, mousemanager, and display concurrently
      (displayController || interactionManager)()
    }
}
