package io.threadcso.debug

import io.threadcso.process._

import scala.util.matching
import scala.util.matching.Regex

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



  /**
    *  A generalized glob pattern, implemented as a regular expression:
    *  {{{
    *  A glob is a sequence of elements
    *  An element is one of the special elements
    *     *     -- matching any sequence of characters
    *     ?     -- matching any single character
    *     [...] -- matching any single character of ...
    *     [^...] -- matching any single character not of ...
    *     \{    -- matching the literal '{'
    *     \,    -- matching the literal ','
    *     \}    -- matching the literal '}'
    *     \*    -- matching the literal '*'
    *     \?    -- matching the literal '?'
    *     \\    -- matching the literal '\'
    * or a braced sequence of globs, any of which it matches
    *     { glob1, glob2, ... }
    * or any non-special single character which matches literally
    * or any unpaired '}', which matches literally.
    * }}}
    *
    * The notation is "forgiving": any unclosed
    * braced sequences are automatically closed.
    *
    * Examples:
    * {{{
    *   water?oo
    *   {water,{l??,f*d}}
    *   \{sequence\,of\,globs\}
    * }}}
    *
    *  TODO: check well-formedness of the pattern
    *
    */
  case class GLOB(val pattern: String, suffix: String="") {
    override val toString: String = if (pattern endsWith suffix) pattern else s"$pattern$suffix"
    val regex = wildCard(toString)
    def apply(string: String): Boolean = regex matches string
    def isEmpty:  Boolean = pattern.isEmpty
    def nonEmpty: Boolean = pattern.nonEmpty

    /**
      * Transforma `glob`-style pattern into a rgeular expression.
      * Alternatives are listed between `{}` and separated by ','; and
      * these three characters can appear in patterns, provided they
      * are "escaped"" by `'\'`; which can itself be escaped.
      */
    def wildCard(glob: String): Regex = {
      var s = new StringBuilder()
      var nest    = 0
      var escape  = false
      var charseq = false
      for {c <- glob} c match {
        case '\\' if !escape => escape = true
        case '*'  if !escape => s.append(".*")
        case '?'  if !escape => s.append(".")
        case ','  if !escape =>
          if (nest > 0) s.append("|") else s.append(c)
        case '{' if !escape =>
          nest += 1
          s.append("(")
        case '[' if !escape =>
          charseq = true
          s.append('[')
        case '^' if !escape && charseq =>
          s.append('^')
        case ']' if !escape =>
          charseq = false
          s.append(']')
        case '}' if !escape =>
          if (nest > 0) {
            nest -= 1
            s.append(")")
          } else
            s.append("\\}") // TODO: BAD WILDCARD SYNTAX INDULGED!
        case _ =>
          if (escape) escape = false
          if ("\\.[]{}()<>*+-=!?^$|".contains(c)) // regex specials
            s.append(s"\\$c")
          else
            s.append(c)
      }

      // TODO: BAD WILDCARD SYNTAX INDULGED!
      while (nest > 0) {
        nest -= 1
        s.append(")")
      }

      new matching.Regex(s.toString())
    }
  }

  private var NOTPROC = GLOB("") // Empty means exclude nothing
  private var PROC    = GLOB("") // Empty means include everything

  private def includeProc(procName: String): Boolean =
    (PROC.isEmpty || PROC(procName)) && !( NOTPROC.nonEmpty && NOTPROC(procName))

  /** Frames to be suppressed in stack backtrace match this pattern */
  private var NOTFRAME = GLOB (
     pattern = io.threadcso.basis.getPropElse("io.threadcso.debug.elided") {
       io.threadcso.basis.getPropElse("io.threadcso.debug.suppress") {
         """{java.,scala.reflect,scala.tools.nsc,*jdk.internal}"""
       }
     },
     suffix = "*")


  ////////////////////////////////

  private def showStackTrace(toDepth: Int, thread: Thread, out: PrintWriter) = {
    val trace = thread.getStackTrace
    var elided  = 0 // number elided by match
    var native  = 0 // number elided as native 
    var depth   = 0 // current depth measured from top
    var shown   = 0 // number shown
    val frames  = trace.length // depth of the trace
    @inline def frameDepth: Int = frames-depth

    for (
      frame <- trace
    ) {
      if (frame.isNativeMethod) native += 1
      if (NOTFRAME(frame.toString))
        elided += 1
      else
        if (shown < toDepth) {
          shown += 1
          out.println(f"${frameDepth}%6d ${unmangle(frame.toString)}")
        }
      depth += 1
    }
    out.print(s" Stack frames: $frames")
    if (native!=0) out.print(s", $native native")
    if (elided!=0) out.print(s", $elided matched ($NOTFRAME)")
    out.println("\n")
  }



  /** unmangle a compiler-generated mangled name */
  private def unmangle(name: String): String = scala.reflect.NameTransformer.decode(name)

  /** Write a textual representation of the current state of the CSO program
    * (its threads and channel and monitored variables) to `out`.
    */
  def showCSOState(toDepth: Int, features: List[String], out: PrintWriter): Unit = {
    import io.threadcso.basis._
    import io.threadcso.debug.REGISTRY.Debuggable

    import java.lang.Thread.State.TIMED_WAITING
    import java.util.concurrent.locks.LockSupport

    //  The mapping from waiting threads to the objects they are waiting in
    //  This is a relic of the time when we used jdk semaphores/locks, and needed to register debuggables
    //  components in order for stack backtraces to be intelligible.
    //  These days we use our own interface to the scheduler, and a descheduled process records
    //  the (often debuggable) component on whose behalf it has been blocked.
    val waiting    = io.threadcso.debug.REGISTRY.waiting
    val registered = io.threadcso.debug.REGISTRY.registered
    def printThread(toDepth: Int)(thread: Thread): Unit = {
      if (includeProc(thread.getName)) {
        val V = if (thread.isVirtual) "V" else "K"
        waiting.get(thread) match {
          case Some(things) =>
            for (thing <- things) {
              out.print(s"${V}THREAD ${thread.identity} AWAITING: ")
              try {
                thing.showState(out)
              }
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
            showStackTrace(toDepth, thread, out)

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
                  showStackTrace(toDepth, thread, out)
                }

              case _ =>
                out.println(s"${V}THREAD ${thread.identity} ${state.toString}")
                showBlocker()
                showStackTrace(toDepth, thread, out)
            }
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
    if (features.contains("threads")) {
      CSOThreads.forActiveKThreads(printThread(toDepth)(_))
      CSOThreads.forActiveVThreads(printThread(toDepth)(_))
    }

    out.println()
    if (monitored.nonEmpty && features.contains("monitored")) {
      out.println("== Monitored Expressions ==")
      for (name <- monitored.keys) {
        out.print(s"$name: ")
        monitored.get(name) match {
          case Some(state) =>
            try {
              out.println(state())
            } catch {
              case t: Throwable => out.println(t)
            }

          case None => out.println("<not available>")
        }
      }
    }

    lazy val announce = out.println("== Registered Objects ==")
    if (features.contains("reg")) {
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
  }

  private def noContentResponse(outputStream: OutputStream): Unit = {
    val reply = new PrintWriter(outputStream, true)
    val stringBuf = new StringWriter()
    val response = new PrintWriter(stringBuf)
    reply.println("HTTP/1.0 204")
    reply.flush()
    reply.close()
  }

  /**
    *
    * Generate an html response on the given output stream
    *
    */
  private def htmlResponse(outputStream: OutputStream)(title: String)(body: HTML.Tree*): Unit = {
    import HTML._
    val reply = new PrintWriter(outputStream, true)
    reply.println("HTTP/1.0 200")
    reply.println("Content-type: text/html; charset=UTF-8")
    reply.println("Server-name: CSO debugger")

    // The space efficiency could be improved here
    // by not accumulating the entire content string
    // But this simplifies generating

    val theHtml: Tree =
        Html() (
          Head()(
            Title()(title),
            Meta("charset"     -> "utf8"),
            Meta("description" -> "CSO Debugger output")
          ),
          Body() (body)
        )
    val content = theHtml.toString

    reply.println("Content-length: " + content.length())
    reply.println("")
    reply.println("<!DOCTYPE html>")
    reply.println(content)
    reply.flush()
    reply.close()
  }


  private def htmlEncodedState(toDepth: Int, features: List[String]): HTML.Tree = {
    import HTML._
    val stringBuf = new StringWriter()
    val response = new PrintWriter(stringBuf)
    showCSOState(toDepth, features, response)
    response.close()
    Div() (
      Par(){ B(s"CSO State ($port) ${new java.util.Date}") },
        Form("method"->"get")(
          Input("type"->"hidden", "name" -> "dashboard", "value"->"true"),
          Par()(Submit()("Back to the dashboard")),
        ),
        Pre() {
        Code() {
          stringBuf.toString
        }
      }
    )
  }

  /** Obsolete: pure text feature response  */
  private def featureResponse(toDepth: Int, features: List[String], outputStream: OutputStream): Unit = {
    //
    // The default response
    //
    val reply = new PrintWriter(outputStream, true)
    val stringBuf = new StringWriter()
    val response = new PrintWriter(stringBuf)
    reply.println("HTTP/1.0 200")
    reply.println("Content-type: text/plain; charset=UTF-8")
    reply.println("Server-name: CSO debugger")


    ///////////////////////////////////////////////

    response.println(s"CSO State ($port) ${new java.util.Date}")
    showCSOState(toDepth, features, response)
    response.flush()

    val content = stringBuf.toString
    reply.println("Content-length: " + content.length())
    reply.println("")


    reply.println(content)
    reply.flush()
    reply.close()
  }

  def dashBoardResponse(outputStream: OutputStream): Unit = {
    import HTML._
    val heading = s"CSO Debugger Dashboard for localhost:$port ${new java.util.Date}"
    htmlResponse(outputStream)(heading)(
      Par()(B(heading)),
      Form("method"->"get") (
        Paragraphs() ("Select features to be shown"),
        Checkbox("features", "threads", "Stack backtraces of running CSO processes", true),
        Div("style" -> "margin: 20px; line-height: 1.2") (
          TextInput("proc",    PROC.pattern,     "Include processes matching:", selected=true),
          TextInput("notproc", NOTPROC.pattern,  "but not processes matching:", selected=true),
          Radio("stackdepth", "1", "Only the top frame", true),
          Radio("stackdepth", "5", "Only the top 5 frames", false),
          Radio("stackdepth", "20", "Only the top 20 frames", false),
          Radio("stackdepth", "10000000", "All frames", false),
          TextInput("elide", NOTFRAME.pattern, "Omit frames matching:", selected=true),
        ),
        Checkbox("features", "monitored", "Monitored Expressions", true),
        Checkbox("features", "reg", "Registered Objects", checked = true),
        Par()(Submit()("Show the selected features")),
      ),
      helpText
    )
  }

  def helpText: HTML.Tree = {
      import HTML._
      val help =
        s"""
          |
          |${B("Stack backtraces")} enables showing a backtrace of each of the running CSO processes
          |whose names match the inclusion criteria specified above. If both (glob) patterns are empty
          |then all processes are included.
          |
          |Stack frames are omitted that match the elision (glob) pattern, specified above. This is initially specified by the
          |system property <code>io.threadcso.debug.elided</code>.
          |
          |<b>Monitored Expressions</b> enables showing the current values of expressions that are being
          |monitored, because <tt>DEBUGGER.monitor(<i>key</i>, <i>expression</i>)</tt>
          |has been invoked to establish the monitoring, and it hasn't been cancelled
          |individually (by <tt>DEBUGGER.monitor(<i>key</i>)</tt>), or en-masse (by <tt>DEBUGGER.monitor()</tt>).
          |
          |<b>Registered Objects</b> enables showing the state of all objects registered with the debugger. These
          |include any open channel, and any user-defined object, <i>o</i> of type <tt>Debuggable</tt> that is, for the moment
          |registered because <tt><i>o</i>.withDebugger(true){<i>body</i>}</tt> was invoked, and <i>body</i> is still being
          |evaluated.
          |
          |""".stripMargin
      Paragraphs()(help)
  }

  // Sledgehammer to show request and header
  private val debuggingResponse = false

  private def handle(sock: Socket) = {
    try {
      val in = new BufferedReader(new InputStreamReader(sock.getInputStream))
      val requested = in.readLine()
      val REFERRER = "Referer: " // [sic]

      var req: List[String]   = requested.split(' ').toList
      var ref: Option[String] = None

      /** Gather the request and header lines */
      val header = new StringBuilder("\n")
      header.append(requested)
      header.append("\n")
      var line = ""
      var reading = true
      while (reading) {
        line = in.readLine()
        if (line==null) {
           reading = false
        }
        else {
          if (line startsWith REFERRER) ref = Some(line.substring(REFERRER.length))
          header.append(line)
          header.append("\n")
          reading = !line.equals("")
        }
      }
      header.append("\n")

      if (debuggingResponse) {
        println(req)
        println(header)
      }

      //
      // Build and send the response
      //
      req match {
        case List("GET", "/favicon.ico", _) =>
             noContentResponse(sock.getOutputStream)
        case List("GET", "/", _) =>
             dashBoardResponse(sock.getOutputStream)
        case List("GET", s"/?$path", _) =>
             val fields = path.trim.split('&').toList
             var features: List[String] = List()
             var stackDepth = 1
             var dashBoard = false
             for { field <- fields } field match {
               case s"features=$f"   => features = f :: features
               case s"stackdepth=$digits"  =>
                 if (digits.matches("[0-9]+")) stackDepth = digits.toInt
               case s"elide=$globpattern" =>
                    NOTFRAME = GLOB(pattern=java.net.URLDecoder.decode(globpattern, "utf8"), suffix="*")
               case s"notproc=$globpattern" =>
                    NOTPROC = GLOB(java.net.URLDecoder.decode(globpattern, "utf8"))
               case s"proc=$globpattern" =>
                    PROC = GLOB(java.net.URLDecoder.decode(globpattern, "utf8"))
               case s"dashboard=$anything" =>
                    dashBoard = true
               case _ => {}
             }
             if (debuggingResponse) println(features)
             // featureResponse (stackDepth, features, sock.getOutputStream)
          if (dashBoard)
             dashBoardResponse(sock.getOutputStream)
          else
             htmlResponse(sock.getOutputStream)(s"CSO Debugger${new java.util.Date}")(htmlEncodedState(stackDepth, features))
        case other =>
             println(req)
             noContentResponse(sock.getOutputStream)
      }
      //
      //
      //
      sock.close()
    } catch {
      case e: IOException =>
        System.out.println("Failed response to client request: " + e.getMessage)
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
