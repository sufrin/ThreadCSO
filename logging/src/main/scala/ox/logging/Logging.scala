package ox.logging
import io.SourceLocation.SourceLocation

/**
  * Management of logging and logging levels
  * for named instances of `Log` and `Logging`.
  *
  * This logging subsystem is intended for use in situations
  * where small numbers of informational messages will be produced
  * -- usually (though not necessarily) destined for the controlling terminal.
  * For higher-resolution logging integrated with the built-in
  * debugger see the package `io.threadcso.debug`.
  *
  */
object Logging
{
  import scala.annotation.elidable
  import scala.annotation.elidable._

  /** The default `logMethod` shows the names of logs */
  var showLogNames: Boolean = false
  /** Show stack backtraces at the point at which `Throwable`s are logged.
    * `Throwable` messages are still passed to the current `loggingMethod`. */
  var showLogStacks: Boolean = true

  /** Constructor for logging messages  */
  case class Message(level: Int, logName: String, message: String, location: SourceLocation)

  /** The current logging method: default value prints the `Message` details `Console.err`. */
  private
  var _loggingMethod: Message  => Unit = {
    case Message(level, name, message, loc) =>
      if (showLogNames)
        Console.err.println(f"${levelName(level)}%-8s $name%-10s $loc%s\n\t $message%s")
      else
        Console.err.println(f"${levelName(level)}%-8s $loc%-18s: $message%s")
  }

  /** Return the current global logging method */
  def loggingMethod = synchronized {
    _loggingMethod
  }

  /** Logs a throwable, and if `showLogStacks` is true, also prints the stack backtrace immediately. */
  def logThrowable(level: Int, name: String, exn: Throwable, loc: SourceLocation): Unit = {
    loggingMethod(Message(level, name, exn.toString, loc))
    // PRO-TEM
    if (showLogStacks) exn.printStackTrace(scala.Console.err)
  }

  /**
      Replace the current global `loggingMethod`, returning its former value.

      The global `loggingMethod` is the last conduit through which logging messages are funelled
      before being output or recorded. Its default value prints the message on `Console.err`.
      (thread-safe)
  */
  def logMethod(method: Message => Unit): ( Message => Unit) =  synchronized
  { val former = loggingMethod
    if (method != null) _loggingMethod = method
    former
  }

  /** Evaluate `body` with `logMethod(method)` then restore the current `loggingMethod` */
  def withLogMethod(method: Message => Unit)(body: => Unit): Unit = {
    val oldMethod = logMethod(method)
    try body finally logMethod(oldMethod)
  }

  /** Maps the name of a logging level to human-readable form -- mnemonic if possible.*/
  def levelName(level: Int): String =
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
     case other     => s"LOGLEVEL($other)"
   }



  /** Maps names `s` to the logging levels that ''will'' be
      associated with them ''when the first logging call is
      made'' from an instantiated `Log(s)` or an
      instantiated class which mixes in `Logging{val
      name=s}`. The mapping defaults to the level associated
      with `ALL`; and this can be set by a call to
      `setLevel("ALL", level)`.
  */

  private
  val levelMap = new scala.collection.mutable.HashMap[String,Int]

  val logMap = new scala.collection.mutable.HashMap[String, Log]

  def Log()(implicit loc: SourceLocation): Log = Log(loc.file)

  def Log(name: String): Log =
    logMap.get(name) match {
      case Some(logging) => logging
      case None =>
        val logging = new Log(name)
        logMap.put(name, logging)
        logging
    }

  override def toString(): String =
    levelMap.map { case (name, level) => s"$name=${levelName(level)}"} . mkString("Logging::\n ", "\n ", " ")

  /** Associates the symbol `name` with the logging level `level`. Such associations
      should be made before the first logging methods are invoked on `Logging`/`Log`
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
      (at least) once by all `Logging` objects as their first logging method
      is called.

      If the level hasn't been set explicitly during the creation of the log, then
      the system property list is examined for properties called `name`, or
      `[name]` with a value that names any logging level.

      {{{
        scala -DSSLTransport=finest -D[TypedTransport]=fine ...
      }}}
  */
  def getLevel(name: String): Int =
  { levelMap.get(name) match
    { case None        => getSystemLevel(name)
      case Some(level) => level
    }
  }

  def getSystemLevel(name: String): Int = {
    import io.threadcso.basis.getPropElse
    getPropElse(s"$name", { level => elidable.byName(level.toUpperCase)}) {
      getPropElse(s"[$name]", { level => elidable.byName(level.toUpperCase) })(DEFAULTLEVEL)
    }
  }

}

/** A mixin to provide various levels of logging.
 *
 *The hierarchy of levels is numeric, but some points have symbolic names.
 *
 *If the `logLevel` of an object into which this mixin has been
    *incorporated is set to a particular level, then all messages
    *strictly below that level are suppressed. Logging methods (except off and all)
    *are named as in the following list (strictly increasing in level).
    *Levels have the same name as the corresponding methods.
    *{{{
        *off
        *finest
        *finer
        *fine
        *info
        *config
        *warning
        *severe
        *all
 *
 *}}}
 *
 *
 *Each of these methods accepts a ''thunk'' `(log: =>String)`
    *rather than a `String`. The effect of this is that the `log`
    *message isn't computed ''unless, on account of the logging level,
    *it is going to be output''. For example, the code
    *{{{
        *finest(s"$estimated nanoseconds to $event")
    *}}}
    *doesn't cause the (possibly lengthy) message-formatting to be
    *done unless the current logging level is `<=finest`.
*/
trait Logging
{ /** The name by which a logging object that incorporates this will be known. */
  val name: String
  import scala.annotation.elidable._

  /** The logging level of this log */
  @inline def logLevel: Int    = Logging.getLevel(name)
  /** Is this log active for levels below FINE */
  @inline def logging: Boolean = logLevel<=FINE

  override def toString = s"Log{name=$name, logLevel=${Logging.levelName(logLevel)}, logging=$logging)"

  /** If the given `level` is not strictly below the current `logLevel`
      for the object being logged, then the given `message` is
      computed and sent out through the (universal) `Logging.loggingMethod`.
  */
  @inline def show(level: Int, message: => String)(implicit loc: SourceLocation): Unit =
          { if (logLevel<=level) { Logging.loggingMethod(Logging.Message(level, name, message, loc)) } }
  @inline def show(level: Int, message: Throwable)(implicit loc: SourceLocation): Unit =
          { if (logLevel<=level) { Logging.logThrowable(level, name, message, loc) } }

  @inline def fine(message:    =>String)(implicit loc: SourceLocation): Unit  =      { show(FINE, message)(loc) }
  @inline def finer(message:   =>String)(implicit loc: SourceLocation): Unit  =      { show(FINER, message)(loc) }
  @inline def finest(message:  =>String)(implicit loc: SourceLocation): Unit  =      { show(FINEST, message)(loc) }
  @inline def info(message:    =>String)(implicit loc: SourceLocation): Unit  =      { show(INFO, message)(loc) }
  @inline def config(message:  =>String)(implicit loc: SourceLocation): Unit  =      { show(CONFIG, message)(loc) }
  @inline def warning(message: =>String)(implicit loc: SourceLocation): Unit  =      { show(WARNING, message)(loc) }
  @inline def severe(message:  =>String)(implicit loc: SourceLocation): Unit  =      { show(SEVERE, message)(loc) }

  @inline def fine(thrown:    Throwable)(implicit loc: SourceLocation): Unit  =       { show(FINE, thrown)(loc) }
  @inline def finer(thrown:   Throwable)(implicit loc: SourceLocation): Unit  =       { show(FINER, thrown)(loc) }
  @inline def finest(thrown:  Throwable)(implicit loc: SourceLocation): Unit  =       { show(FINEST, thrown)(loc) }
  @inline def info(thrown:    Throwable) (implicit loc: SourceLocation): Unit =       { show(INFO, thrown)(loc) }
  @inline def config(thrown:  Throwable)(implicit loc: SourceLocation): Unit  =       { show(CONFIG, thrown)(loc) }
  @inline def warning(thrown: Throwable)(implicit loc: SourceLocation): Unit  =       { show(WARNING, thrown)(loc) }
  @inline def severe(thrown:  Throwable)(implicit loc: SourceLocation): Unit  =       { show(SEVERE, thrown)(loc) }

}