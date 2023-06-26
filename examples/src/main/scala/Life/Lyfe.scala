import app.OPT._
import display._
import io.threadcso._
import io.threadcso.channel.PortState


/**
  *
  *  Life implementation in which the cells exchange states along transport.
  *  This is a challenge to the raw speed of channel implementations and to the
  *  scheduling algorithms for the virtual thread implementation that underlies
  *  CSO processes (post 2023). 
  *
  */
object Lyfe extends App {

  val Command = "Lyfe"
  type Cell = LyfeCell

  class CellArray(cols: Int, rows: Int) extends RectangularArray[Cell](cols, rows) {
        @inline def forSelected(effect: Cell=> Unit): Unit =
          for { cell <- this if cell.selected } effect(cell)

        @inline def forEach(effect: Cell=> Unit): Unit =
          this.foreach(effect)
  }

  object CellArray {
    def apply[T](cols: Int, rows: Int)(init: (Int, Int) => Cell): CellArray = {
      val a = new CellArray(cols, rows)
      for {c <- 0 until cols; r <- 0 until rows}
        a(c, r) = init(c, r)
      a
    }
  }

  /** The array of cells -- initialized when dimensions are known  */
  var allCells: CellArray = null
  @inline def forSelectedCells(effect: Cell => Unit): Unit = allCells.forSelected(effect)
  @inline def forEachCell(effect: Cell => Unit): Unit = allCells.forEach(effect)
  @inline def forCells(xs: Seq[Int], ys: Seq[Int])(effect: Cell=> Unit): Unit = {
     for { row <- ys; col <- xs if allCells.definedAt(col, row) } effect(allCells(col, row))
  }
  @inline def forCell(col: Int, row: Int)(effect: Cell => Unit): Unit = {
    if (allCells.definedAt(col, row)) effect(allCells(col, row)) else {}
  }

  var _cols  = 50
  var _rows  = 40
  var width  = 1000
  var height = 800
  var fast   = false

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
    OPT("[0-9]+", { arg => _rows= arg.toInt; _cols = _rows; width = 10* _rows; height=10* _rows }, "rows and columns of 10x10 cells" ),
    OPT("-f", fast, "Use experimental 'fast' transport in cells."),
    OPT("-h", { Usage() }, "prints usage text"),
  )

  override def Usage(): Unit = {
    super.Usage()
    Console.err.println(
      """
        |The dashboard is self-explanatory.
        |
        |When the simulation is stopped:
        | Mouse click      selects the cell the mouse points to.
        | Ctrl-mouseclick  inverts the selection of the cell the mouse points to.
        |
        | mousedrag        selects the cells dragged over
        | ctrl-mousedrag   deselects the cells dragged over
        |
        | Ctrl-Delete/Backspace kills all cells.
        | Delete/Backspace      kills all selected cells.
        |
        | Left/Right/Up/Down
        |       selects the cell leftwards, rightwards, upwards, downwards
        |       from the last cell selected. Use these keys to select lines.
        |
        | Z     selects about 1/5th of the cells, randomly chosen.
        | 6     switches the birth/survival regime between B3/S23 and B63/S23.
        | Space brings to life all the selected cells and starts the simulation;
        |
        |When the simulation is running:
        | Space stops the simulation
        | Mouse click stops the simulation
        |
        |""".stripMargin)
  }


  var GUI: LyfeDashboard[Cell] = null

  @inline private def running: Boolean = GUI.running

  var RADIUS     = 0.0
  var GENERATION = 0

    /**
      *   Process that reacts to all display events
      */
  val interactionManager = proc("interactionManager") {
    var lastX, lastY, mouseX, mouseY   = 0

    def toLast[D](mouse: MouseEventDetail[D]): Unit = {
        lastX = (mouse.x / RADIUS).toInt
        lastY = (mouse.y / RADIUS).toInt
    }

    def toMouse[D](mouse: MouseEventDetail[D]): Unit = {
      mouseX = (mouse.x / RADIUS).toInt
      mouseY = (mouse.y / RADIUS).toInt
    }

    var down = false

      repeat {
        GUI.fromDisplay ? {

          case Dragged(mouse) if !running =>
            val STATE = !(mouse.isControl || mouse.isMeta)
            toLast(mouse)
            val cols = (lastX min mouseX) until (lastX max mouseX)+1
            val rows = (lastY min mouseY) until (lastY max mouseY)+1
            forCells(cols, rows) { _.selected = STATE }
            GUI.refresh()

          case Pressed(mouse) if running =>
            toMouse(mouse)
            GUI.setRunning(false)
            StatusReport()

          case Pressed(mouse) if !running =>
            toMouse(mouse)
            val CONTROL = mouse.isControl || mouse.isMeta
            val lastHits = GUI.display.at(mouse.x, mouse.y, false)

            // add to / remove from the selection
            if (CONTROL)
               for (body <- lastHits) {
                 body.selected = !body.selected
               }

            // add to the selection
            if (!CONTROL) {
                for (cell <- lastHits)  { cell.selected = true }
            }
            GUI.refresh()

          case KeyPressed(key) =>
            var changed = true
            val CONTROL = key.isControl || key.isMeta
            import java.awt.event.KeyEvent._
            key.code match {


              case VK_RIGHT if !running =>
                mouseX = (mouseX + 1) min (allCells.cols-1)
                allCells(mouseX, mouseY).selected = !CONTROL
              case VK_LEFT if !running =>
                mouseX = (mouseX-1) max 0
                allCells(mouseX, mouseY).selected = !CONTROL
              case VK_UP if !running =>
                mouseY = (mouseY-1) max 0
                allCells(mouseX, mouseY).selected = !CONTROL
              case VK_DOWN if !running =>
                mouseY = (mouseY + 1) min (allCells.rows-1)
                allCells(mouseX, mouseY).selected = !CONTROL

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

              case VK_DELETE | VK_BACK_SPACE if !running && CONTROL =>
                forEachCell { cell =>
                  cell.lifeTime = 0
                }
                GENERATION = 0

              case VK_DELETE | VK_BACK_SPACE if !running && !CONTROL=>
                forSelectedCells { cell =>
                    cell.selected = false
                    cell.lifeTime = 0
                }
                GENERATION = 0

              case VK_Z if !running =>
                  val random = new scala.util.Random
                  for (_ <- 0 until allCells.elements / 5) {
                    val cell = random.nextInt(allCells.elements)
                    allCells(cell).selected = true
                  }

              case VK_6 if (!running) =>
                   forEachCell { cell => cell.b6 = !cell.b6 }

              case _ =>
                   StatusReport()
            }
            if (changed) GUI.refresh()

          case _ =>
            {}
        }
      }
    }

    def StatusReport() = {
      val alive = allCells.filter { _.lifeTime>0 }
      val longLived = alive.filter { _.lifeTime >2 }
      val verylongLived = longLived.filter { _.lifeTime >6 }
      GUI.report(s"Generation $GENERATION: of ${allCells.elements} cells, ${alive.size} are alive; ${longLived.size} alive >2; ${verylongLived.size} alive >6")
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

    /**
      *
      * An experimental and partial implementation of very fast transport
      * (This isn't needed for the implementation; the experiment is to see
      *  if it is worthwhile)
      *
      */
    class FastChan[T](name: String) extends lock.primitive.DataChan[T](name) with Chan[T] {
      /** Signal that the channel is to be closed forthwith */
      override def close(): Unit = ???
      override def readBefore(ns: Milliseconds): Option[T] = ???
      override def ??[U](f: T => U): U = ???
      override def closeIn(): Unit = ???
      override def canInput: Boolean = ???
      override def inPortState: PortState = ???
      override def registerOut(alt: alternation.Runnable, theIndex: Int): PortState = ???
      override def unregisterOut(): PortState = ???
      override def registerIn(alt: alternation.Runnable, theIndex: Int): PortState = ???
      override def unregisterIn(): PortState = ???
      /** Discover the name generator */
      override def nameGenerator: basis.NameGenerator = ???
      override def writeBefore(nsWait: Milliseconds)(value: T): Boolean = ???
      override def closeOut(): Unit = ???
      override def canOutput: Boolean = ???
      override def outPortState: PortState = ???

      /** Block until a value is available for input, then read and return it. */
      override def ?(): T = read()
      override def ?(ignored: Unit): T = read()
    }

    /** Channel factory for interCell communication transport  */
    def makeChan[T](name: String): Chan[T] =
      if (fast) new FastChan(name) else OneOne(name)

    def Main(): Unit = {
      println(debugger)


      val w = width  / _cols
      val h = height / _rows
      val R = w min h

      val cols   = width / R
      val rows   = height / R

      RADIUS     = R

      /**
        *
        * Topologically the cell field is a torus represented as an
        * array: with westmost and eastmost columns considered to be
        * adjacent, as well as northmost and southmost columns.
        *
        * Each cell has 8 link transport, one to each of its neighbours.
        * Neighbours are (by convention) numbered clockwise from 0 to 7.
        * Neighbour 0 is the "north-west" neighbour; neighbour 1 is
        * the "north" neighbour; ... neighbour 7 is the "west" neighbour.
        * This convention is respected by `RectangularArray.neighbours(c, r)`,
        * but it doesn't really matter what the starting compass direction is.
        */

        val linkOut = RectangularArray[IndexedSeq[Chan[Boolean]]](cols, rows) {
          (c, r) => for (i <- RectangularArray.Neighbourhood) yield makeChan[Boolean](s"($c,$r)($i)")
        }


      /**
        * The input links of each cell are the output
        * links of its corresponding neighbour, to wit:
        * neighbour `i` for output is neighbour `(i+4)%8`
        * for input. The computation of this "view" of the links is
        * straightforward.
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
        val linkIn = RectangularArray[IndexedSeq[Chan[Boolean]]](cols, rows) {
          (c, r) =>
            val hood = linkOut.neighbourhood(c, r).iterator
            // assert: the count of `hood` is `Neighbours`
            (for { i <- linkOut.Neighbourhood } yield hood.next()((i+4)%linkOut.Neighbours)).toIndexedSeq
        }


      // set up the cells
      allCells = CellArray(cols, rows) {
        case (c, r) =>  new LyfeCell(c * R, r * R, R, R, linkIn(c, r), linkOut(c, r))
      }

      // Sends a `Sync` to each cell, concurrently
      val syncCells: PROC = || (for { cell <- allCells.toList } yield π { cell.instructions ! Sync })

      val displayController: PROC = proc("DisplayController") {

            // start the cell controllers
            forEachCell(cell=>cell.controller.fork)

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

      GUI = new LyfeDashboard(allCells, title = "Lyfe", width = width, height = height)

      // run the bodies, mousemanager, and display concurrently
      (displayController || interactionManager)()
    }
}
