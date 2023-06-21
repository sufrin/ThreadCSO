package  ox.logging
import ox.logging.Logging.logMap


/** A concrete Log that registers as `_name` */
class  Log(_name: String="")(implicit loc: io.SourceLocation.SourceLocation) extends Logging {
  val name: String = _name match {
    case "" => loc.file
    case _  => _name
  }
  locally {
    logMap.put(name, this)
  }
}



object Log {
  def apply(name: String) = Logging.Log(name)
}




