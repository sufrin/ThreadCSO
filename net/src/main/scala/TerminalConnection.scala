package ox.net

import io.threadcso
import io.threadcso.process.Process
import io.threadcso.{OneOne, PROC, component, proc, repeat}

/**  A connection that relays I/O to the controlling terminal. Input comes line-by-line from the keyboard,
  *  and output is printed on the screen.
  */
object TerminalConnection extends Connection[String, String] with PROC {
  val log = ox.logging.Log("TerminalConnection")
  private val fromKbd = OneOne[String]("From Keyboard")
  private val toConsole = OneOne[String]("To Console")

  val out: threadcso.!![String] = toConsole
  val in: threadcso.??[String] = fromKbd

  /** Process handle on a running `outRelay`, if any */
  var outDaemon: Process.Handle = null
  /** Process handle on a running `inRelay`, if any */
  var inDaemon: Process.Handle = null

  val inRelay = component.keyboard(fromKbd, "")
  val outRelay = proc("toConsole") {
    repeat { toConsole ? { s => Console.println(s) } }
  }

  /** Run the relaying processes */
  def apply(): Unit = synchronized {
    outDaemon = outRelay.fork
    inRelay()
  }

  /** Fork the relaying processes */
  def fork: Process.Handle = synchronized {
    outDaemon = outRelay.fork
    inDaemon = inRelay.fork
    inDaemon
  }

  /**  Close the transfer channels and interrupt the relaying processes.
    *  The connection cannot be used once it has been closed.
    */
  def close(): Unit = synchronized {
    if ((inDaemon ne null) && (outDaemon ne null)) {
      log.fine("TerminalConnection.close()")
      fromKbd.close()
      toConsole.close()
      inDaemon.interrupt()
      outDaemon.interrupt()
      inDaemon = null
      outDaemon = null
    }
  }
}
