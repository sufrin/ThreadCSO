import io.threadcso._
import app.OPT._

import scala.collection.mutable.TreeMap

/** A program to ''digest'' the texts in a collection of files identified by
  * their (local) paths or URLs.
  */

object Digesticator extends App {

  /** A `Job` embodies a path (or url) and the pattern that is used to separate
    * its words
    */
  case class Job(
      var separate: String = "[^a-zA-Z]+",
      var include: String = "",
      var exclude: String = "()",
      var path: String = ""
  ) {
    override def toString: String = s"""$path"""
  }

  // The jobs to be done
  val jobs = new collection.mutable.Queue[Job]
  // Prototype of a job to be done -- command line prameters change the prototype; paths/urls copy it into the queue
  var job = Job()
  // Number of file/url reader processes
  var readers = 1
  // Number of word digester processes
  var digesters = 1
  // Size of a file buffer
  var bufferSize = 128
  // Start the debugger
  var debugging = false
  // List the words alphabetically on termination
  var list = false

  val Options = List(
    OPT("-s", job.separate, "«regexp» word-separator pattern ($separate)"),
    OPT("-i", job.include, "«regexp» words to include ($include)"),
    OPT("-x", job.exclude, "«regexp» words to exclude ($exclude)"),
    OPT("-r", readers, "«int» number of readers"),
    OPT("-d", digesters, "«int» number of digesters"),
    OPT("-b", bufferSize, "«int» buffer size"),
    OPT("-l", list, true, "list word frequencies"),
    OPT("-D", debugging, true, "start debugger"),
    ELSE(
      "<path>|<url>",
      { path => jobs.enqueue(job.copy(path = path)) },
      "adds a path/url to the list to be processed"
    )
  )

  val Command = "Digest"

  def Main(): Unit = {
    val toReaders =
      N2NBuf[Job](size = 200, writers = 1, readers = readers, "toReaders")
    val toDigesters: Seq[Chan[String]] =
      for (me <- 0 until digesters)
        yield N2NBuf[String](
          size = 200,
          writers = readers,
          readers = 1,
          name = s"toDigesters($me)"
        )

    val count =
      for (me <- 0 until digesters)
        yield new scala.collection.mutable.HashMap[String, Int]

    if (debugging) println(debugger)
    val startTime = nanoTime
    (proc("controller") {
      repeatFor(jobs) { job => toReaders ! job }
      toReaders.closeOut()
    }
      || ||(
        for (me <- 0 until readers) yield reader(me, toReaders, toDigesters)
      )
      || ||(
        for (me <- 0 until digesters)
          yield digester(me, toDigesters(me), count(me))
      ))()
    val elapsed = nanoTime - startTime
    var total = 0
    for (me <- 0 until digesters) total += count(me).size
    val sorted = new TreeMap[String, Int]()(ord = PhoneBookOrder)
    for (me <- 0 until digesters) sorted ++= count(me)

    if (list) for ((w, c) <- sorted) println(f"$w%-40s $c%7d")
    println(
      s"-r $readers -d $digesters: ${elapsed.hms} (${jobs.size} files, ${sorted.size} words / $total digested)"
    )
    exit()
  }

  /** Returns the sequence of words of the source text located at `path`. The
    * method of parsing the text is extremely crude (at present), but need not
    * be so.
    *
    * @param path
    *   the path/url of the source
    * @param splitPattern
    *   the pattern to be used to separate the text of the source into ''words''
    * @return
    *   the sequence of ''words'' in the source text (empty if text cannot be
    *   located)
    */
  def words(path: String, splitPattern: String): Seq[String] = {
    import java.io.File

    import scala.io.Source
    try {
      if (path.matches(".*//.*"))
        Source.fromURL(path).mkString.split(splitPattern).toIndexedSeq
      else
        Source.fromFile(new File(path), bufferSize).mkString.split(splitPattern).toIndexedSeq
    } catch {
      case exn: java.io.FileNotFoundException =>
        Console.err.println(s"File not found: $path"); Array[String]().toIndexedSeq
      case exn: java.net.ConnectException =>
        Console.err.println(s"Connection refused: $path"); Array[String]().toIndexedSeq
      case exn: Exception => Console.err.println(s"$exn"); Array[String]().toIndexedSeq
    }
  }

  /** A process that repeatedly accepts and performs jobs
    *
    * @param me
    *   reader's id
    * @param jobs
    *   the stream of jobs to be done
    * @param outs
    *   the streams to which words will be written
    */
  def reader(me: Int, jobs: ??[Job], outs: Seq[!![String]]) =
    proc(s"reader($me") {
      val size = outs.size
      @inline def route(word: String): Int = word(0).toInt % size

      repeat {
        val job = jobs ? ()
        val parsed = words(job.path, job.separate)
        val noinc = job.include == ""
        val noexc = job.exclude == ""
        repeatFor(parsed) { word =>
          if (
            (noexc || !word.matches(job.exclude)) &&
            (noinc || word.matches(job.include))
          ) outs(route(word)) ! word
        }
      }

      for (out <- outs) out.closeOut()
      jobs.closeIn()
    }

  /** A process that reads words from `words` and puts them (with their
    * frequency count) in a mapping
    *
    * @param me
    *   digester's id
    * @param words
    *   stream of words
    * @param count
    *   mapping from words to frequencies
    */
  def digester(
      me: Int,
      words: ??[String],
      count: scala.collection.mutable.Map[String, Int]
  ) = proc(s"digester($me") {
    repeat {
      val w = words ? () // toUpperCase()
      count.put(w, 1 + count.getOrElse(w, 0))
      ()
    }
  }

  /** Ordering which places each lowercase letter just after its corresponding
    * uppercase letter. Assumption: each Char of any Unicode plane can be
    * represented in no more than 30 bits. In fact the Unicode code points are
    * in the range `0x000000` to `0x10FFFF` and can be represented with 24 bits.
    */
  object PhoneBookOrder extends scala.math.Ordering[String] {

    /** Yield collation code of `c` */
    @inline def collate(c: Char): Int =
      if (c.isUpper) (c.toLower << 1) else 1 + (c << 1)

    /** Lexicographic comparison of strings by the collation codes of their
      * characters
      */
    def compare(x: String, y: String): Int = {
      var diff = 0
      var i = 0
      val xs = x.size
      val ys = y.size
      val cs = if (xs <= ys) xs else ys
      while (i < cs && x(i) == y(i)) i += 1 // scan common prefix
      // diff==0 && (i==cs || x(i)!=y(i))
      while (i < cs && diff == 0) {
        diff = collate(x(i)) - collate(y(i))
        i += 1
      }
      // i==cs || diff!=0
      // (i==cs => the shorter string is a prefix of the longer)
      if (i == cs) xs - ys else diff
    }
  }

}
