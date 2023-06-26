


import display.Displayable
import io.threadcso._

import java.awt.{Color, Graphics2D}

trait Instruction
case object Sync extends Instruction
case object Die  extends Instruction
case object Live extends Instruction

/**
  *  A cell has transport to and from its neighbours.
  */
class LyfeCell(  val x: Double,
                 val y: Double,
                 val w: Double,
                 val h: Double,
                 val fromNbr:      Seq[??[Boolean]],
                 val toNbr:        Seq[!![Boolean]],
              ) extends Displayable {

  /*
   * TODO: A heuristic that might improve redisplaying speed by
   *  not changing what's shown of a cell if it has not changed. This
   *  cannot be implemented JUST at the cell level, for at the moment
   *  repainting the screen relies on each cell painting its own
   *  background (because the screen is cleared at a draw). Getting it
   *  right will involve changes to `Display`.
   */

  /**
    * Number of proper neighbours
    */
  val neighbours        = 8

  /** Number of generations this cell has been "alive" */
  var lifeTime: Int     = 0

  /** Birth on 6 neighbours as well as 3 neighbours */
  var b6 = false

  /** Selected from the GUI  */
  var selected: Boolean = false

  var count = 0

  /**
    * This process simultaneously broadcasts this cell's state to its neighbours
    * and reads its individual neighbours' states.
    * Because *reading* is done sequentially there is no need to
    * capture the individual neighbours' states so counting is
    * done directly.
    */
  val exchangeStates: PROC = {
    val broadcast = ||(for { nbr <- toNbr} yield π { nbr ! (lifeTime>0)})
    val update    = proc { count = 0; for { i <- 0 until neighbours } fromNbr(i)?{ case true => count+=1; case false => } }
    (broadcast || update)
  }


  override def paintOn(g: Graphics2D, toPixels: Double=>Int): Boolean = {
    import Color._
    val c = lifeTime match {
      case 0             => if (b6) GRAY else LIGHT_GRAY
      case 1 | 2         => GREEN
      case 3 | 4 | 5 | 6 => RED
      case _             => BLUE
    }
    g.setColor(c)
    g.fillRect(toPixels(x), toPixels(y), toPixels(w), toPixels(h))
    g.setColor(Color.WHITE)
    g.drawRect(toPixels(x), toPixels(y), toPixels(w), toPixels(h))
    if (selected) {
      g.fillRect(toPixels(x)+2, toPixels(y)+2, toPixels(w)-2, toPixels(h)-2)
    }
    true
  }

  val instructions: Chan[Instruction] = new Lyfe.FastChan[Instruction](s"FastChan($x,$y)")

  /**
    *   The controlling PROC of this cell
    */
  val controller: PROC = proc {
    repeat (true) {
      instructions ? {
        case Live  => { lifeTime = 1 }
        case Die   => { lifeTime = 0 }
        case Sync  =>
          exchangeStates()
          lifeTime = lifeTime match {
            case 0 =>  count match {
              case 3       => 1
              case 6 if b6 => 1
              case _       => 0
            }

            case _ => count match {
              case 2 | 3 => lifeTime + 1
              case _     => 0
            }
          }
          lifeTime = count match {
            case 2 | 3 => if (lifeTime>0) lifeTime + 1 else 0
            case 6     => if (b6) 1 else 0
            case _     => 0
          }

      }
    }
  }

}
