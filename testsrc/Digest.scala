
import io.threadcso._
import ox.app.OPT._
import scala.collection.mutable.TreeMap

/**
  * A program to ''digest'' the texts in a collection of files identified by their (local) paths
  * or URLs. Digesting just counts occurrences of individual words. The main point of the program is
  * to demonstrate that there is some advantage in allocating more processes than there are processors
  * to a task that is input/output bound. This is demonstrated most effectively when digesting a sequence
  * of files identified by (remote) URLs.
  */

object Digest extends App
{ /** A `Job` embodies a path (or url) and the pattern that is used to separate its words */
  case class Job
  (
    var separate: String  = "[^a-zA-Z]+",
    var path:     String  = ""
  )
  {
    override def toString: String = s"""$path"""
  }

  // The jobs to be done
  val jobs       = new collection.mutable.Queue[Job]
  // Prototype of a job to be done
  var job        = Job()
  // Number of file/url reader processes
  var readers    = 1
  // Number of word digester processes
  var digesters  = 1
  // Size of a file buffer
  var bufferSize = 128
  // Start the debugger
  var debugging  = false
  // List the words alphabetically on termination
  var list       = false


  val Options = List (
    OPT("-s", job.separate, "«regexp» word-separate pattern ($separate)"),
    OPT("-r",      readers,            "«int» number of readers"),
    OPT("-d",      digesters,          "«int» number of digesters"),
    OPT("-b",      bufferSize,         "«int» buffer size"),
    OPT("-l",      list,       true,   "list word frequencies"),
    OPT("-D",      debugging,  true,   "start debugger"),
    ELSE("<path>|<url>", { path => jobs.enqueue(job.copy(path=path)) }, "adds a path/url to the list to be processed")
  )

  val Command = "Digest"

  def Main: Unit =
  { val toReaders  = N2NBuf[Job](size=200, writers=1, readers=readers, "toReaders")
    val toDigesters: Seq[Chan[String]] =
                     if (readers==digesters)
                       for (me <- 0 until digesters) yield
                         OneOneBuf[String](size=2000, s"toDigesters($me)")
                     else
                       List(N2NBuf[String](size=2000, writers=readers, readers=digesters, "toDigester"))

    def toDigester(me: Int) = if (readers==digesters) toDigesters(me) else toDigesters(0)

    val count      = for (me <- 0 until digesters) yield
                         new scala.collection.mutable.HashMap[String, Int]

    if (debugging) println(debugger)
    val startTime = nanoTime
    ( proc("controller")
             { repeatFor (jobs) { job => toReaders!job }
               toReaders.closeOut()
             }
      || || (for (me <- 0 until readers)   yield reader(me, toReaders, toDigester(me)))
      || || (for (me <- 0 until digesters) yield digester(me, toDigester(me), count(me)))
      )()
    val elapsed = nanoTime - startTime
    var total = 0
    for (me <- 0 until digesters) total += count(me).size
    val sorted = new TreeMap[String, Int]()(IgnoreCaseOrdering)
    for (me <- 0 until digesters) sorted  ++= count(me)
    if (list) for ((w, c) <- sorted) println(f"$w%-40s $c%7d")
    println(s"-r $readers -d $digesters: ${elapsed.hms} (${jobs.size} files, ${sorted.size} words / $total digested)")
    exit()
  }

  /**
    * Returns the sequence of words of the source text located at `path`. The method
    * of parsing the text is extremely crude (at present), but need not be so.
    *
    * @param path the path/url of the source
    * @param splitPattern the pattern to be used to separate the text of the source into ''words''
    * @return the sequence of ''words'' in the source text (empty if text cannot be located)
    */
  def words(path: String, splitPattern: String): Seq[String] =
  { import scala.io.Source
    import java.io.File
    try
    {
      if (path.matches(".*//.*"))
        Source.fromURL(path).mkString.split(splitPattern)
      else
        Source.fromFile(new File(path), bufferSize).mkString.split(splitPattern)
    }
    catch
    {
      case exn: java.io.FileNotFoundException => Console.err.println(s"File not found: $path"); Array[String]()
      case exn: java.net.ConnectException => Console.err.println(s"Connection refused: $path"); Array[String]()
      case exn: Exception => Console.err.println(s"$exn"); Array[String]()
    }
  }

  /**
    * A process that repeatedly accepts and performs jobs
    *
    * @param me reader's id
    * @param jobs the stream of jobs to be done
    * @param out  the stream to which each job's words will be written
    */
  def reader(me: Int, jobs: ?[Job], out: ![String]) = proc (s"reader($me")
  {
    repeat {
      val job    = jobs ? ()
      val parsed = words(job.path, job.separate)
      repeatFor(parsed) { w => out!w }
    }
    out.closeOut()
    jobs.closeIn()
  }

  /**
    * A process that reads words from `words` and puts them (with their frequency count) in a mapping
    *
    * @param me digester's id
    * @param words stream of words
    * @param count mapping from words to frequencies
    *
    */
  def digester(me: Int, words: ?[String], count: scala.collection.mutable.Map[String, Int]) = proc (s"digester($me")
  {
    repeat {
      val w = words?()
      count.put(w, 1+count.getOrElse(w, 0))
      ()
    }
  }

  object IgnoreCaseOrdering extends scala.math.Ordering[String]
  {
    def compare(x: String, y: String) = x.compareToIgnoreCase(y)
  }

}
