package io.threadcso.net

import io.threadcso
import io.threadcso.net.transport.Connection
import io.threadcso.process.Process
import io.threadcso.{ManyOne, OneOne, PROC, proc, repeat}

import java.io.{FileInputStream, FileOutputStream, InputStreamReader, OutputStreamWriter}

/**
  *  A connection that relays I/O to and from the controlling terminal
  *  (`/dev/tty` on unix-like operating systems.).
  *
  *  Input comes line-by-line from the keyboard, but omits the newline character at the end
  *  of the line.
  *
  *  Output is printed on the screen. When it is non-null, `EOF` is sent if (and only if) the
  *  input stream is closed (by typing ctrl-D on unix-like operating systems). It's a good
  *  idea (though not essential) to make `EOF` a string that will not otherwise be sent,
  *  namely one with a newline in it.
  *
  *  I/O  is performed by a couple of daemon processes that are started when the connection
  *  is opened, and terminated when the connection is closed.
  */
class TerminalConnection(EOF: String = null) extends Connection[String, String] with PROC {
  val log = ox.logging.Log("TerminalConnection")
  private val fromKbd   = OneOne[String]("From Keyboard")
  private val toConsole = ManyOne[String]("To Console")

  val out: threadcso.!![String] = toConsole
  val in: threadcso.??[String]  = fromKbd

  /** Process handle on a running `outRelay`, if any */
  var outDaemon: Process.Handle = null
  /** Process handle on a running `inRelay`, if any */
  var inDaemon: Process.Handle = null

  val kbd     = new InputStreamReader(new FileInputStream("/dev/tty"), "UTF-8")
  val console = new OutputStreamWriter(new FileOutputStream("/dev/tty"), "UTF-8")
  var reading = true

  val inRelay = proc ("from Keyboard") {
    def readLine(): Option[String] = {
      val res   = new StringBuilder()
      var ch = kbd.read()
      while (ch != '\n' && ch != -1) {
        ch match {
          case -1 =>
          case _  => res.addOne(ch.toChar); ch = kbd.read()
        }
      }
      if (ch == -1) None else Some(res.toString)
    }
    while (reading) {
           readLine() match {
              case None       => if (EOF ne null) fromKbd!EOF; reading = false
              case Some(line) => fromKbd!line
           }
    }
    fromKbd.close()
  }

  val outRelay = proc("to Console") {
    repeat { toConsole ? { s => console . write(s); console.flush(); } }
  }

  /** Run the relaying processes */
  def apply(): Unit = synchronized {
    outDaemon = outRelay.fork
    inRelay()
  }

  /** Start the relaying processes */
  def open(): Unit = { fork; () }

  /** Fork the relaying processes */
  def fork: Process.Handle = synchronized {
    outDaemon = outRelay.fork
    inDaemon = inRelay.fork
    inDaemon
  }

  /**  Close the transfer transport and interrupt the relaying processes.
    *  The connection cannot be used once it has been closed.
    */
  def close(): Unit = synchronized {
    if ((inDaemon ne null) && (outDaemon ne null)) {
      log.finer("TerminalConnection.close()")
      fromKbd.close()
      toConsole.close()
      inDaemon.interrupt()
      outDaemon.interrupt()
      inDaemon = null
      outDaemon = null
    }
  }
}
