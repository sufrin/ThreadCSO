package ox.eieio

import io.SourceLocation.SourceLocation



/**
  * Methods to provide management of logging and logging levels
  * to specific instances of `Logger` or `Logging`.
*/
object Logging 
{
  import scala.annotation.elidable
  import scala.annotation.elidable._

  /** 
      Replace the current global logging method, returning its former value.
      
      The logging method is the last conduit through which
      logging messages are funelled. It is called
      with `(level, name, message)`.
      
      (thread-safe)
  */
  def setLogMethod(method: (Int,String,String, SourceLocation) => Unit): ( (Int, String, String, SourceLocation) => Unit) =  synchronized
  { val former = loggingMethod
    if (method != null) _loggingMethod = method
    former
  }
  
  /** Maps the name of a logging level to human-readable form -- mnemonic if possible.*/
  @inline def levelName(level: Int): String =
   level match
   { 
     case FINEST    => "FINEST"
     case FINER     => "FINER"
     case FINE      => "FINE"
     case INFO      => "INFO"
     case WARNING   => "WARNING"
     case SEVERE    => "SEVERE"
     case ASSERTION => "ASSERTION"
     case ALL       => "ALL"
     case OFF       => "OFF"
     case other     => s"LOG($other)"
   }
     
  var showLogNames:         Boolean = false
  var showLoggedExceptions: Boolean = false

  private 
  var _loggingMethod: (Int, String, String, SourceLocation) => Unit =
      { case (level, name, message, loc) =>
        if (showLogNames)
           Console.println(f"${levelName(level)}%-8s $name%-10s $loc%s\n\t $message%s")
        else
           Console.println(f"${levelName(level)}%-8s $loc%-18s: $message%s")
      }

  /** Return the current logging method (thread-safe) */
  def loggingMethod = synchronized { _loggingMethod }
  
  /** Logs a throwable */
  def loggingThrowableMethod(level: Int, name: String, exn: Throwable, loc: SourceLocation): Unit =
  { loggingMethod(level, name, exn.toString, loc)
    // PRO-TEM
    if (showLoggedExceptions) exn.printStackTrace
  }
  
  /** Maps names `s` to the logging levels that ''will'' be
      associated with them ''when the first logging call is
      made'' from an instantiated `Logger(s)` or an
      instantiated class which mixes in `Logging{val
      name=s}`. The mapping defaults to the level associated
      with `ALL`; and this can be set by a call to
      `setLevel("ALL", level)`.
  */

  private
  val levelMap = new scala.collection.mutable.HashMap[String,Int]

  override def toString(): String =
    levelMap.map { case (name, level) => s"$name=${levelName(level)}"} . mkString(" ", "\n ", " ")
  
  /** Associates the symbol `name` with the logging level `level`. Such associations
      should be made before the first logging methods are invoked on `Logging`/`Logger`
      objects with the given `name`; else they will be given the default
      logging level current at the time of the first logging call.
  
  */
  def setLevel(name: String, level: String): Unit =
  { val lev = 
      if (level.matches("[0-9]+")) level.toInt
      else                         elidable.byName(level.toUpperCase)
      if (name=="ALL")
         DEFAULTLEVEL=lev
      else
         levelMap.put(name, lev)
  }
  
  var DEFAULTLEVEL = elidable.byName("WARNING")
  
  /** Get the logging level associated with `name`. This is called
      once by all `Logging` objects as their first logging method
      is called.
  */
  def getLevel(name: String): Int =
  { levelMap.get(name) match
    { case None        => DEFAULTLEVEL
      case Some(level) => level
    }
  }
  
}

/** A mixin to provide various levels of logging.

    The hierarchy of levels is numeric, but some points have symbolic names.
    
    If the `logLevel` of an object into which this mixin has been
    incorporated is set to a particular level, then all messages
    strictly below that level are suppressed. Logging methods (except off and all)
    are named as in the following list (strictly increasing in level).
    Levels have the same name as the corresponding methods.    
    {{{ off
        finest
        finer
        fine
        info
        config
        warning
        severe
        all
    }}}  
    
    Each of these methods accepts a ''thunk'' `(log: =>String)` 
    rather than a `String`. The effect of this is that the `log`
    message isn't computed ''unless, on account of the logging level,
    it is going to be output''. For example, the code
    {{{
        finest(s"$estimated nanoseconds to $event")
    }}}
    doesn't cause the (possibly lengthy) message-formatting to be 
    done unless the current logging level is `<=finest`.
*/
trait Logging
{ /** The name by which a logging object that incorporates this will be known. */
  val name: String
  import scala.annotation.elidable
  import scala.annotation.elidable._
  
  /** The logging level of this log; established at the first invocation of a logging method */
  lazy val logLevel : Int = Logging.getLevel(name)
  
  /** Set the current logging level (by name or as an integer) */
  
  override def toString = ("Logging{name=%s, logLevel=%d}".format(name, logLevel))

  /** If the given `level` is not strictly below the current `logLevel`
      for the object being logged, then the given `message` is
      computed and sent out through the (universal) `Logging.loggingMethod`.
  */
  @inline def show(level: Int, message: => String)(implicit loc: SourceLocation): Unit =
          { if (logLevel<=level) { Logging.loggingMethod(level, name, message, loc) } }
  @inline def show(level: Int, message: Throwable)(implicit loc: SourceLocation): Unit =
          { if (logLevel<=level) { Logging.loggingThrowableMethod(level, name, message, loc) } }
                 
  @inline def fine(log:    =>String)(implicit loc: SourceLocation): Unit  =      { show(FINE, log)(loc) }
  @inline def finer(log:   =>String)(implicit loc: SourceLocation): Unit  =      { show(FINER, log)(loc) }
  @inline def finest(log:  =>String)(implicit loc: SourceLocation): Unit  =      { show(FINEST, log)(loc) }
  @inline def info(log:    =>String)(implicit loc: SourceLocation): Unit  =      { show(INFO, log)(loc) }
  @inline def config(log:  =>String)(implicit loc: SourceLocation): Unit  =      { show(CONFIG, log)(loc) }
  @inline def warning(log: =>String)(implicit loc: SourceLocation): Unit  =      { show(WARNING, log)(loc) }
  @inline def severe(log:  =>String)(implicit loc: SourceLocation): Unit  =      { show(SEVERE, log)(loc) }

  @inline def fine(log:  Throwable)(implicit loc: SourceLocation): Unit  =       { show(FINE, log)(loc) }
  @inline def finer(log:  Throwable)(implicit loc: SourceLocation): Unit  =       { show(FINER, log)(loc) }
  @inline def finest(log: Throwable)(implicit loc: SourceLocation): Unit  =       { show(FINEST, log)(loc) }
  @inline def info(log:   Throwable) (implicit loc: SourceLocation): Unit =       { show(INFO, log)(loc) }
  @inline def config(log: Throwable)(implicit loc: SourceLocation): Unit  =       { show(CONFIG, log)(loc) }
  @inline def warning(log: Throwable)(implicit loc: SourceLocation): Unit =       { show(WARNING, log)(loc) }
  @inline def severe(log: Throwable)(implicit loc: SourceLocation): Unit  =       { show(SEVERE, log)(loc) }
  
}

/** A concrete Logger that registers as `_name` */
class  Logger(_name: String="")(implicit loc: io.SourceLocation.SourceLocation) extends Logging {
  val name= _name match {
    case "" => loc.file
    case _  => _name
  }
}

object Logger {
  def apply(name: String): Logger = new Logger(name)
}






