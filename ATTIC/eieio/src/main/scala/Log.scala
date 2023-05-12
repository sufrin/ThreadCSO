package  ox.logging

/** A concrete Log that registers as `_name` */
class  Log(_name: String="")(implicit loc: io.SourceLocation.SourceLocation) extends Logging {
  val name: String = _name match {
    case "" => loc.file
    case _  => _name
  }
}

object Log {
  def apply(_name: String="")(implicit loc: io.SourceLocation.SourceLocation): Log = new Log(_name)
}






