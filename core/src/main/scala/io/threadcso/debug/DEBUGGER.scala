package io.threadcso.debug

import io.threadcso.process.CSOThreads

/** A simple server that responds to browser requests with a description of the
  * state of active `CSO` threads, open channel, and monitored
  * values/expressions.
  *
  * To ensure that the server is loaded and started it is necessary for code of
  * the following spirit to be executed on program startup.
  *
  * {{{
  * val debugger = new io.threadcso.DEBUGGER(<some port number>)
  * }}}
  *
  * See also the definition of [[io.threadcso.debugger]] for an account of how
  * this is usually done automatically (with a randomly-allocated free port
  * number). This is essentially by mentioning the lazy variable
  * io.threadcso.debugger`. For example, the following code prints an
  * announcement that the debugger is running {{{ import io.threadcso._ ... if
  * (debugging) println(debugger) }}} in the form:
  * `Debugger(http://localhost:64601)`
  * -- which a browser can easily be pointed at.
  */
class DEBUGGER(debugPort: Int = 0) {
  import java.io._
  import java.net.{ServerSocket, Socket}

  import scala.collection.concurrent.TrieMap

  override def toString = s"Debugger(http://localhost:$port)"

  /** Make a thread with the given name and body */
  private def thread(name: String)(body: => Unit) = {
    val res = new Thread(name) { override def run(): Unit = body }
    res.setPriority(Thread.MAX_PRIORITY)
    res.setDaemon(false)
    res
  }

  /** The port number on which the debugger thread will listen for browser
    * connections. Unless this port number is `0` the debugger is started. If
    * not set by the jdk/scala property `io.threadcso.debugger.port=...` then
    * this defaults to the `debugPort` class parameter (which itself defaults to
    * `0`).
    */
  var port: Int =
    io.threadcso.basis.getPropElse("io.threadcso.debugger.port", _.toInt)(
      debugPort
    )

  if (port >= 0) {
    val socket = new ServerSocket(port)
    port = socket.getLocalPort

    thread("CSO Debugger Server (Listening on port %d)".format(port)) {
      while (true) {
        val sock = socket.accept
        thread("CSO Debugger Responder") {
          handle(sock)
        }.start()
      }
    }.start()
  }

  /** Frames to be suppressed in stack backtrace match this pattern */
  private val SUPPRESS = io.threadcso.basis.getPropElse(
    "io.threadcso.debug.suppress"
  )("""^j(ava|dk).util.concurrent.lock.*""")

  private def showStackTrace(thread: Thread, out: PrintWriter) = {
    val trace = thread.getStackTrace
    for (
      frame <- trace
      if !frame.isNativeMethod && !frame.getClassName.matches(SUPPRESS)
    ) { out.println(unmangle(frame.toString)) }
    out.println("")
  }

  /** Mapping from mangleable characters to their mangling. */
  private[this] val mangleMap = List(
    ("~", "$tilde"),
    ("=", "$eq"),
    ("<", "$less"),
    (">", "$greater"),
    ("!", "$bang"),
    ("#", "$hash"),
    ("%", "$percent"),
    ("^", "$up"),
    ("&", "$amp"),
    ("|", "$bar"),
    ("*", "$times"),
    ("/", "$div"),
    ("+", "$plus"),
    ("-", "$minus"),
    (":", "$colon"),
    ("\\", "$bslash"),
    ("?", "$qmark"),
    ("@", "$at")
  )

  /** unmangle a compiler-generated mangled name */
  private def unmangle(name: String): String = {
    var r = name
    for ((ch, mangled) <- mangleMap) r = r.replace(mangled, ch)
    r
  }

  /** Write a textual representation of the current state of the CSO program
    * (its threads and channel and monitored variables) to `out`.
    */
  def showCSOState(out: PrintWriter): Unit = {
    import java.lang.Thread.State.TIMED_WAITING
    import java.util.concurrent.locks.LockSupport

    import io.threadcso.basis._
    import io.threadcso.debug.REGISTRY.Debuggable

    
    //  The mapping from waiting threads to the objects they are waiting in
    //  This is a relic of the time when we used jdk semaphores/locks, and needed to register debuggables
    //  components in order for stack backtraces to be intelligible.
    //  These days we use our own interface to the scheduler, and a descheduled process records
    //  the (often debuggable) component on whose behalf it has been blocked.
    val waiting = io.threadcso.debug.REGISTRY.waiting
    val registered = io.threadcso.debug.REGISTRY.registered

    def printThread(thread: Thread): Unit = {
      val V = if (thread.isVirtual) "V" else "K"
      waiting.get(thread) match {
        case Some(things) =>
          for (thing <- things) {
            out.print(s"${V}THREAD ${thread.identity} AWAITING: ")
            try { thing.showState(out) }
            catch {
              case t: Throwable =>
                out.println(
                  "Exception while showing the state of a registed component"
                )
                t.printStackTrace(out)
                out.println("----------------")
            }
            out.println()
          }
          showStackTrace(thread, out)

        case _ =>
          val state = thread.getState
          val blocker = LockSupport.getBlocker(thread)
          def showBlocker() = try {
            if (blocker != null)
              blocker match {
                case obj: Debuggable =>
                  if (obj.hasState) {
                    out.print(" FOR ")
                    obj.showState(out)
                    out.println()
                  }
                case other =>
                  out.print(" FOR ")
                  out.println(other.toString)
              }
          } catch {
            case t: Throwable =>
              out.println("Exception while determining state of a blocker")
              t.printStackTrace(out)
          }
          state match {
            case TIMED_WAITING =>
              out.print(s"${V}THREAD ${thread.identity} ${state.toString}")
              if (thread.getName.matches("^cso-pool.*"))
                out.println(" (in thread pool)")
              else {
                showBlocker()
                showStackTrace(thread, out)
              }

            case _ =>
              out.println(s"${V}THREAD ${thread.identity} ${state.toString}")
              showBlocker()
              showStackTrace(thread, out)
          }
      }
    }

    // Experimental: account for deadlocks
    // Commented out: not useful in (Oracle) JVM 1.[78]
    // (because lock ownership isn't set by jdk.util.concurrent.ReentrantLock)
    /*
      val MX = jdk.lang.management.ManagementFactory.getThreadMXBean()
      val threadinfo = MX.dumpAllThreads(true, true)
      for (i<-0 until threadinfo.size)
      {  val t = threadinfo(i)
         t.getThreadState match
         { case NEW => {}
           case TERMINATED => {}
           case _ => out.println(s"${t.getThreadName} waiting for ${t.getLockName} owned by ${t.getLockOwnerId}")
         }
      }
     */

    // Print out the states of the running threads
    CSOThreads.forActiveKThreads(printThread(_))
    CSOThreads.forActiveVThreads(printThread(_))


    out.println()
    if (monitored.nonEmpty) {
      out.println("== Monitored Expressions ==")
      for (name <- monitored.keys) {
        out.print(s"$name: ")
        monitored.get(name) match {
          case Some(state) =>
            try {
              out.println(state())
            } catch { case t: Throwable => out.println(t) }

          case None => out.println("<not available>")
        }
      }
    }

    lazy val announce = out.println("== Registered Objects ==")
    for ((_, sobj) <- registered) {
      val obj = sobj.get
      if (obj != null) {
        announce
        try {
          if (obj.hasState) {
            out.println()
            obj.showState(out)
          }
        } catch {
          case t: Throwable =>
            out.println(
              "Exception while determining state of a registered object"
            )
            t.printStackTrace(out)
        }
        // out.println
      }
    }
  }

  private def handle(sock: Socket) = {
    try {
      val in = new BufferedReader(new InputStreamReader(sock.getInputStream))
      val request = in.readLine()

      /** Right now we ignore the request and the header lines */
      val header = new StringBuilder("<tt>\n")
      header.append(request)
      header.append("<br/>\n")
      var line = ""
      var reading = true
      while (reading) {
        line = in.readLine()
        header.append(line)
        header.append("<br/>\n")
        reading = !line.equals("")
      }
      header.append("</tt>\n")

      val reply = new PrintWriter(sock.getOutputStream, true)
      val stringBuf = new StringWriter()
      val response = new PrintWriter(stringBuf)
      reply.println("HTTP/1.0 200")
      reply.println("Content-type: text/plain; charset=UTF-8")
      reply.println("Server-name: CSO debugger")

      response.println(s"CSO State ${new java.util.Date}")
      showCSOState(response)
      response.flush()

      val content = stringBuf.toString
      reply.println("Content-length: " + content.length())
      reply.println("")
      reply.println(content)
      reply.flush()
      reply.close()
      sock.close()
    } catch {
      case e: IOException =>
        System.out.println("Failed respond to client request: " + e.getMessage)
    } finally {
      if (sock != null) {
        try { sock.close() }
        catch { case e: IOException => e.printStackTrace() }
      }
    }
  }

  private val monitored = new TrieMap[String, () => String]

  /** Associate `name` with a string expression to be evaluated and reported
    * when the state of the program is reported to a browser by the debugger.
    */
  def monitor(name: String, state: => String): Unit =
    monitored += ((name, { case () => state }))

  /** Remove the association of `name` to any string expression it was
    * associated with.
    */
  def monitor(name: String): Unit = monitored -= name

  /** Clear all monitored expressions.
    */
  def monitor(): Unit = monitored.clear()

}
