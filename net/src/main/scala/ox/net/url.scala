package ox.net
import app.OPT._

import java.io.{BufferedReader, InputStreamReader}
import java.net.URI


object url extends App {
  import ox.logging._

  import java.net.URL

  var url: URL = null
  var src: String = ""

  var error: Option[Throwable] = None

  /** The list of `Opt`ions that define the syntax and semantics of each of this
    * command line application's options.
    */
  val Options = List[Opt](
       OPT("-s", src,  s"<name> set src")
    ,  OPT(".+", { u => url = new URI(u).toURL(); () }, "<url>")
  )
  /** The name of this command line application.
    *
    * See above for an example.
    */
  override val Command: String = "url"

  /**
    * The command invoked once the command-line options have been parsed.
    */
  override def Main(): Unit = {
    println(s"url: $url")
    val conn = url.openConnection()
    println(s"url: $conn")
    // SET OPTIONS HERE
    conn.setAllowUserInteraction(true)
    //
    try { conn.connect()  } catch { case exn: Exception => error = Some(exn) }
    if (error.isEmpty) {
      val stream = conn.getInputStream()
      val reader = new BufferedReader(new InputStreamReader(stream))
      var line: String = null
      var going = true
      while (going) {
        line = reader.readLine()
        if (line == null) going = false else println(line)
      }
      reader.close()
    } else {
      println(s"Error: ${error.get}")
    }

  }
}
