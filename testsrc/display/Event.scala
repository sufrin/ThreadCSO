package display

import java.awt._

trait EventDetail[D]
{ /** milliseconds after the epoch when this event occured */
  val when      : Long
  /** Was this modifier button pressed when this event occured */
  val isAlt     : Boolean
  /** Was this modifier button pressed when this event occured */
  val isAltGraph: Boolean
  /** Was this modifier button pressed when this event occured */
  val isControl : Boolean
  /** Was this modifier button pressed when this event occured */
  val isMeta    : Boolean
  /** Was this modifier button pressed when this event occured */
  val isShift   : Boolean
}

/**
  * The mouse moved and/or a button on it was pressed or released. A `Clicked` event is taken to be a
  * pair of `Pressed` `Released` events in sequence, repeated possibly many times. All the events are
  * reported: the programmer should choose between responding to `Clicked` or to `Pressed` and `Released`
  * events.
  */
class MouseEventDetail[D](jdk: event.MouseEvent, pixelsPerUnitLength: Double) extends EventDetail[D]
{ /** What (if any) mouse buttons were pressed */
  val button: Int    = jdk.getButton
  /** Model-space coordinate of the position of the mouse */
  val x     : Double = jdk.getX.toDouble / pixelsPerUnitLength
  /** Model-space coordinate of the position of the mouse */
  val y     : Double = jdk.getY.toDouble / pixelsPerUnitLength
  /** How many clicks were made (`Clicked` events only) */
  val clicks: Int    = jdk.getClickCount
  val when  : Long   = jdk.getWhen
  val isAlt          = jdk.isAltDown
  val isAltGraph     = jdk.isAltGraphDown
  val isControl      = jdk.isControlDown
  val isMeta         = jdk.isMetaDown
  val isShift        = jdk.isShiftDown
  override
  def toString: String = s"""${jdk.paramString()}@($x,$y)"""

}

/**   A mousewheel-like control has been rotated. Some operating systems (OS/X in particular) report
  *   two-fingered "dragging" on a tablet as mouse wheel rotations.
  */
class MouseWheelEventDetail[D](jdk: event.MouseWheelEvent, pixelsPerUnitLength: Double)
      extends MouseEventDetail[D](jdk, pixelsPerUnitLength)
{ /** How many wheel-steps (or their equivalent) were made. */
  val rotation: Int =  jdk.getWheelRotation
}

/**
  * A key was typed when the `Display`s window had the focus.
  */
class KeyEventDetail[D] (jdk: event.KeyEvent, pixelsPerUnitLength: Double) extends EventDetail[D]
{ /** Unicode character that was typed */
  val char: Char = jdk.getKeyChar ()
  /** The (Java) key code of the character that was typed */
  val code: Int  = jdk.getKeyCode ()
  val when: Long = jdk.getWhen
  val isAlt      = jdk.isAltDown
  val isAltGraph = jdk.isAltGraphDown
  val isControl  = jdk.isControlDown
  val isMeta     = jdk.isMetaDown
  val isShift    = jdk.isShiftDown
  override def toString: String = jdk.paramString()
}

/** An `Event[D]` captures the essence of a (GUI) event that has been captured by a `Display[D]`. */
trait Event[D] {}

case class Entered[D](detail: MouseEventDetail[D]) extends Event[D]

case class Exited[D](detail: MouseEventDetail[D]) extends Event[D]

case class Pressed[D](detail: MouseEventDetail[D]) extends Event[D]

case class Released[D](detail: MouseEventDetail[D]) extends Event[D]

case class Clicked[D](detail: MouseEventDetail[D]) extends Event[D]

case class Moved[D](detail: MouseEventDetail[D]) extends Event[D]

case class Dragged[D](detail: MouseEventDetail[D]) extends Event[D]

case class KeyPressed[D](detail: KeyEventDetail[D]) extends Event[D]

case class KeyReleased[D](detail: KeyEventDetail[D]) extends Event[D]

case class KeyTyped[D](detail: KeyEventDetail[D]) extends Event[D]

case class Wheel[D](detail: MouseWheelEventDetail[D]) extends Event[D]




