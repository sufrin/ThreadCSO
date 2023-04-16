

object Message {

  /** Messages from the interface to individual particles */
  trait Message
  case object Tick extends Message
  case class AddBody(body: Body) extends Message
  case class RemoveBody(body: Body) extends Message
  case class DeltaR(delta: Double) extends Message
  case class DeltaD(delta: Double) extends Message
  case class DeltaV(delta: Double) extends Message

}
