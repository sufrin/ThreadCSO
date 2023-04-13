


import display.Displayable
import io.threadcso._

import java.awt.{Color, Graphics2D}

trait Instruction
case object Sync extends Instruction
case object Die  extends Instruction
case object Live extends Instruction

/**
  *  A cell has channels to and from its neighbours.
  */
class LyfeCell(  val x: Double,
                 val y: Double,
                 val w: Double,
                 val h: Double,
                 val fromNbr:      Seq[??[Boolean]],
                 val toNbr:        Seq[!![Boolean]],
              ) extends Displayable {

  val neighbours        = 8
  var state: Boolean    = false
  var lifetime: Int     = 0
  // Birth on 6 as well as 3
  var b6 = false

  var selected: Boolean = false
  def color: Color      = Color.GREEN

  val neighbourState = new Array[Boolean](neighbours)

  /**
    * This process simultaneously broadcasts this cell's state to its neighbours
    * and reads its individual neighbours' states.
    *
    * If *reading* were done sequentially then there would be no need to
    * capture the individual neighbours' states and counting could be
    * done directly.
    */
  val exchangeStates: PROC = {
    val broadcast = ||(for { nbr <- toNbr}             yield π { nbr ! state})
    val update =    ||(for { i <- 0 until neighbours } yield π { neighbourState(i) = fromNbr(i)?() })
    (broadcast || update)
  }

  /**
    * Returns the next state computed from neighbours' states.
    */
  def nextState: Boolean= {
    var count = 0
    for (s <- neighbourState) if (s) count += 1
    state match {
      case true =>
        count match {
          case 0 => false
          case 1 => false
          case 2 => true
          case 3 => true
          case _ => false
        }
      case false =>
        count == 3 || (b6 && count == 6)
    }
  }

  override def paintOn(g: Graphics2D, toPixels: Double=>Int): Boolean = {
    g.setColor(if (state) (if (lifetime<3) Color.GREEN else Color.RED) else (if (b6) Color.GRAY else Color.LIGHT_GRAY))
    g.fillRect(toPixels(x), toPixels(y), toPixels(w), toPixels(h))
    g.setColor(Color.WHITE)
    g.drawRect(toPixels(x), toPixels(y), toPixels(w), toPixels(h))
    if (selected) {
      g.fillRect(toPixels(x)+2, toPixels(y)+2, toPixels(w)-2, toPixels(h)-2)
    }
    true
  }

  val instructions: Chan[Instruction] = OneOne[Instruction]

  /**
    *   The controlling PROC of this cell
    */
  val controller: PROC = proc {
    repeat (true) {
      instructions ? {
        case Live  => { state = true; lifetime = 0 }
        case Die   => state = false
        case Sync  =>
          exchangeStates()
          val next = nextState
          if (state&&next) lifetime+=1 else lifetime=0
          state = nextState
      }
    }
  }

}
