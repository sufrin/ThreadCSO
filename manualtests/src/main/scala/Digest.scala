import io.threadcso._
import app.OPT._
import scala.collection.mutable.TreeMap

/** A program to ''digest'' the texts in a collection of files identified by
  * their (local) paths or URLs. Digesting just counts occurrences of individual
  * words. The main point of the program is to demonstrate that there is some
  * advantage in allocating more processes than there are processors to a task
  * that is input/output bound. This is demonstrated most effectively when
  * digesting a sequence of files identified by (remote) URLs.
  *
  * Timed trials demonstrate the effects of multiple readers/digesters. The following results
  * are for scala sources of the present package. The first run in every sequence is the
  * "warm-up" run (where JIT compilation of hotspots will tkae place).
  *
  * 1931 % ./runexample Digest -t 5 -r 15 -d 15 `findscala`
  * -r 15 -d 15: (109 files, 3773 words / 11536 digested) Digested, Merged: 00.111,525, 00.130,322 241848237
  * -r 15 -d 15: (109 files, 3773 words / 11416 digested) Digested, Merged: 00.054,520, 00.059,504 114025067
  * -r 15 -d 15: (109 files, 3773 words / 11248 digested) Digested, Merged: 00.026,462, 00.033,379 59842652
  * -r 15 -d 15: (109 files, 3773 words / 10343 digested) Digested, Merged: 00.022,287, 00.029,529 51817892
  * -r 15 -d 15: (109 files, 3773 words / 11142 digested) Digested, Merged: 00.016,317, 00.022,701 39018507
  * 1932 % ./runexample Digest -t 5 -r 15 -d 15 `findscala`
  * -r 15 -d 15: (109 files, 3773 words / 11465 digested) Digested, Merged: 00.106,739, 00.125,898 232638078
  * -r 15 -d 15: (109 files, 3773 words / 11328 digested) Digested, Merged: 00.043,101, 00.048,817 91918533
  * -r 15 -d 15: (109 files, 3773 words / 10691 digested) Digested, Merged: 00.018,660, 00.024,119 42780280
  * -r 15 -d 15: (109 files, 3773 words / 11283 digested) Digested, Merged: 00.018,256, 00.032,133 50390280
  * -r 15 -d 15: (109 files, 3773 words / 11241 digested) Digested, Merged: 00.018,365, 00.025,442 43808044
  * 1933 % ./runexample Digest -t 5 -r 15 -d 15 -b 10000 `findscala`
  * -r 15 -d 15: (109 files, 3773 words / 11673 digested) Digested, Merged: 00.107,656, 00.129,251 236907523
  * -r 15 -d 15: (109 files, 3773 words / 11341 digested) Digested, Merged: 00.041,085, 00.047,693 88778973
  * -r 15 -d 15: (109 files, 3773 words / 11387 digested) Digested, Merged: 00.024,516, 00.033,972 58489205
  * -r 15 -d 15: (109 files, 3773 words / 11196 digested) Digested, Merged: 00.025,392, 00.038,173 63565582
  * -r 15 -d 15: (109 files, 3773 words / 11512 digested) Digested, Merged: 00.023,369, 00.028,839 52209410
  * 1934 % ./runexample Digest -t 5 -r 10 -d 10 -b 10000 `findscala`
  * -r 10 -d 10: (109 files, 3773 words / 10726 digested) Digested, Merged: 00.112,791, 00.133,668 246460387
  * -r 10 -d 10: (109 files, 3773 words / 10739 digested) Digested, Merged: 00.038,255, 00.044,120 82376058
  * -r 10 -d 10: (109 files, 3773 words / 10545 digested) Digested, Merged: 00.022,827, 00.028,038 50866874
  * -r 10 -d 10: (109 files, 3773 words / 10519 digested) Digested, Merged: 00.019,602, 00.030,222 49824608
  * -r 10 -d 10: (109 files, 3773 words / 10729 digested) Digested, Merged: 00.015,154, 00.022,244 37398495
  * 1935 % ./runexample Digest -t 5 -r 10 -d 10 -b 1000 `findscala`
  * -r 10 -d 10: (109 files, 3773 words / 10709 digested) Digested, Merged: 00.097,315, 00.115,750 213065755
  * -r 10 -d 10: (109 files, 3773 words / 10834 digested) Digested, Merged: 00.047,047, 00.053,414 100462407
  * -r 10 -d 10: (109 files, 3773 words / 10360 digested) Digested, Merged: 00.021,772, 00.027,687 49459945
  * -r 10 -d 10: (109 files, 3773 words / 10446 digested) Digested, Merged: 00.015,168, 00.027,952 43120717
  * -r 10 -d 10: (109 files, 3773 words / 10754 digested) Digested, Merged: 00.016,485, 00.021,835 38321822
  */

object Digest extends App {

  /** A `Job` embodies a path (or url) and the pattern that is used to separate
    * its words
    */
  case class Job(
      var separate: String = "[^a-zA-Z]+",
      var path: String = ""
  ) {
    override def toString: String = s"""$path"""
  }

  // The jobs to be done
  val jobs = new collection.mutable.Queue[Job]
  // Prototype of a job to be done
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
  var list   = false

  var trials = 1

  val Options = List(
    OPT("-s", job.separate, "«regexp» word-separate pattern ($separate)"),
    OPT("-r", readers, "«int» number of readers"),
    OPT("-d", digesters, "«int» number of digesters"),
    OPT("-b", bufferSize, "«int» buffer size"),
    OPT("-l", list, true, "list word frequencies"),
    OPT("-t", trials, "«int» number of timed trials (no output)"),
    OPT("-D", debugging, true, "start debugger"),
    OPT("-K", {
      arg: String => scala.util.Properties.setProp("io.threadcso.pool.KIND", arg)
    }, "«poolkind» set process runner kind"),
    ELSE(
      "<path>|<url>",
      { path => jobs.enqueue(job.copy(path = path)) },
      "adds a path/url to the list to be processed"
    )
  )

  val Command = "Digest"

  def Main(): Unit = {
    for { i<-0 until trials } OneRun(list && i==0 && trials==1)
    exit()
  }

  def OneRun(list: Boolean): Unit = {
    val toReaders =
      N2NBuf[Job](size = 200, writers = 1, readers = readers, "toReaders")
    val toDigesters: Seq[Chan[String]] =
      if (readers == digesters)
        for (me <- 0 until digesters)
          yield OneOneBuf[String](size = 2000, s"toDigesters($me)")
      else
        List(
          N2NBuf[String](
            size = 2000,
            writers = readers,
            readers = digesters,
            "toDigester"
          )
        )

    def toDigester(me: Int) =
      if (readers == digesters) toDigesters(me) else toDigesters(0)

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
        for (me <- 0 until readers) yield reader(me, toReaders, toDigester(me))
      )
      || ||(
        for (me <- 0 until digesters)
          yield digester(me, toDigester(me), count(me))
      ))()
    val digested = nanoTime-startTime
    var total = 0
    for (me <- 0 until digesters) total += count(me).size
    val sorted = new TreeMap[String, Int]()(IgnoreCaseOrdering)
    for (me <- 0 until digesters) sorted ++= count(me)
    val merged = nanoTime-startTime
    if (list) for ((w, c) <- sorted) println(f"$w%-40s $c%7d")
    println(
      s"-r $readers -d $digesters: (${jobs.size} files, ${sorted.size} words / $total digested) Digested, Merged: ${digested.hms}, ${merged.hms} ${digested+merged}"
    )
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
    import scala.io.Source
    import java.io.File
    try {
      if (path.matches(".*//.*"))
        Source.fromURL(path).mkString.split(splitPattern) .toIndexedSeq
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
    * @param out
    *   the stream to which each job's words will be written
    */
  def reader(me: Int, jobs: ??[Job], out: !![String]) = proc(s"reader($me") {
    repeat {
      val job = jobs ? ()
      val parsed = words(job.path, job.separate)
      repeatFor(parsed) { w => out ! w }
    }
    out.closeOut()
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
      val w = words ? ()
      count.put(w, 1 + count.getOrElse(w, 0))
      ()
    }
  }

  object IgnoreCaseOrdering extends scala.math.Ordering[String] {
    def compare(x: String, y: String) = x.compareToIgnoreCase(y)
  }

}
