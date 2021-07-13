import io.threadcso._
import ox.app.OPT._

import scala.collection.mutable.TreeMap

/**
  * A program to make a concordance (occurence-list) of the texts in a collection of files identified by their
  * (local) paths or URLs. If the file is an HTML file then only its text, not its markup, is digested.
  *
  * Specifying a word-pattern with (-w) makes possible some useful source-text processing. For
  * example: -w "((class|trait|def)[ ]+.+)|([A-Za-z_]+)" indexes all alphabetical words, and also
  * all scala class, trait, and function definitions.
  *
  * With this in mind, at some point it might be useful to indicate which part of a word-pattern is to be the index
  * string for it; right now the matching text itself is used as its own index string, so definitions of functions with
  * distinct signatures are indexed separately.
  */

object Occurences extends App
{
  /** A `Job` embodies a path (or url) and the pattern that is used to separate its words */
  case class Job
  (
    var wordpat:  String  = "[a-z_A-Z][a-z_A-Z0-9]+",
    var include:  String  = "",
    var exclude:  String  = "()",
    var path:     String  = ""
  )
  {
    override def toString: String = s"""$path"""
  }

  // The jobs to be done
  val jobs       = new collection.mutable.Queue[Job]
  // Prototype of a job to be done -- command line prameters change the prototype; paths/urls copy it into the queue
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
  // Trials
  var trials = 1


  val Options = List (
                       OPT("-w",      job.wordpat,        "«regexp» word-pattern ($wordpat)"),
                       OPT("-i",      job.include,        "«regexp» words to include ($include) -- unused at present"),
                       OPT("-x",      job.exclude,        "«regexp» words to exclude ($exclude) -- unused at present"),
                       OPT("-r",      readers,            "«int» number of readers"),
                       OPT("-d",      digesters,          "«int» number of digesters"),
                       OPT("-b",      bufferSize,         "«int» buffer size ($bufferSize)"),
                       OPT("-l",      list,       true,   "list word locations alphabetically"),
                       OPT("'+D",     debugging,  true,   "start debugger"),
                       OPT("-T",      trials,             "<int> number of times to process the inputs (for average timing)"),
                       ELSE("<path>|<url>", { path => jobs.enqueue(job.copy(path=path)) }, "adds a path/url to the list to be processed")
                     )

  val Command = "Occurences"

  case class Location(line: Int, char: Int, path: String)

  def Main(): Unit =
  {
    if (debugging) println(debugger)
    for (i <- 0 until trials) MainProgram
    exit()
  }


  def MainProgram: Unit =
  { val toReaders  = N2NBuf[Job](size=200, writers=1, readers=readers, "toReaders")
    val toDigesters: Seq[Chan[(String, Location)]] =
      for (me <- 0 until digesters) yield
          N2NBuf[(String, Location)](size=200, writers= readers, readers=1, name= s"toDigesters($me)")


    val digests = for (me <- 0 until digesters) yield
        new scala.collection.mutable.HashMap[String, List[Location]]

    val startTime = nanoTime
    ( proc("controller")
      { repeatFor (jobs) { job => if (debugging) Console.err.println(job); toReaders!job }
        toReaders.closeOut()
      }
      || || (for (me <- 0 until readers)   yield reader(me, toReaders, toDigesters))
      || || (for (me <- 0 until digesters) yield digester(me, toDigesters(me), digests(me)))
      )()
    // words digested
    val digestTime = nanoTime
    var total = 0
    for (me <- 0 until digesters) total += digests(me).size
    val sorted = new TreeMap[String, List[Location]]()(ord= PhoneBookOrder)
    for (me <- 0 until digesters) sorted ++= digests(me)
    val sortedTime = nanoTime

    if (list)
      for ((w, occs) <- sorted)
      { println(f"$w%-40s")
        for (occ<-occs.reverse) println(f"  ${occ.path}%-70s ${occ.line}.${occ.char} ")
      }
    println(s"-T $trials -r $readers -d $digesters: ${(digestTime-startTime).hms} ${(sortedTime-digestTime).hms} ${(sortedTime-startTime).hms} (${jobs.size} files, ${sorted.size} words / $total digested)")
  }

  /**
    * Send the (line-numbered) lines of the source text located at `path` to `out`. If the path
    * signifies that the file is an html file, then its text (without markup) is sent (as a single line, for
    * the html parser we use doesn't identify line numbers)
    *
    * @param path the path/url of the source
    * @param out  the output steam to which the lines are sent
    */

  def lines(path: String, out: ![(Int, String)]): PROC = proc(s"lines($path)")
  { import java.io.File

    import scala.io.Source
    try
    { val lines =
      if (path.matches(".*[Hh][Tt][Mm][Ll]?$"))
           Soup.text(path).split("\n").toList.iterator
      else if (path.matches("^[Hh][Tt][Tt][Pp][Ss]?://"))
           Source.fromURL(path).getLines()
      else
           Source.fromFile(new File(path), bufferSize).getLines()

      var ln = 0
      for (line<-lines) { out!(ln, line); ln+= 1}
    }
    catch
    {
      case exn: java.io.FileNotFoundException => Console.err.println(s"File not found: $path")
      case exn: java.net.ConnectException => Console.err.println(s"Connection refused: $path")
      case exn: Exception => Console.err.println(s"$exn")
    }
    out.closeOut()
  }

  /**
    * Transform an input stream of line-numbered lines into an output streams of words and their locations. Words are
    * determined by the given `wordPattern`, which is a regular expression.
    */
  def words(path: String, wordPattern: String, in: ?[(Int, String)], out: ![(String, Location)], closeOut: Boolean = true): PROC = proc(s"words")
  { import java.util.regex.{Pattern, Matcher}
    val pat = Pattern.compile(wordPattern)
    val mat = pat.matcher("")
    repeat
    { val (ln, line) = in?()
      mat.reset(line)
      while (mat.find())
      { val word = line.substring(mat.start(), mat.end())
        out!(word, Location(ln, mat.start(), path))
      }
    }
    if (closeOut) out.closeOut
    in.closeIn
  }

  /**
    * Yield a process that reads words and their locations from `in` and routes each of them (using a simple hash function)
    * to the appropriate digester.
    *
    */
  def router(in: ?[(String, Location)], toDigesters: Seq[![(String, Location)]]): PROC = proc("router"){
      val size = toDigesters.size
      @inline def route(word: String): Int = (word(0).toInt+word.size) % size
      repeat
      { val occurence = in?()
        toDigesters(route(occurence._1)) ! occurence
      }
  }

  /**
    * Yield a process that routes each of the words (accompanied by its location) found in the file denoted by
    * `path` to a digester determined by a hash function.  Words are determined by the given `wordPattern`, which
    * is a regular expression.
    *
    */
  def parser(path: String, wordPattern: String, toDigesters: Seq[![(String, Location)]]): PROC =
  {
    if (toDigesters.size==1)
      { // optimise the case where no routeing is needed
        val left  = OneOneBuf[(Int, String)](1500, name = s"parser($path).left")
        val right = toDigesters(0)
        lines(path, left) || words(path, wordPattern, left, right, closeOut=false)
      }
    else
    {
      val left = OneOneBuf[(Int, String)](1500, name = s"parser($path).left")
      val right = OneOneBuf[(String, Location)](1500, name = s"parser($path).right")
      lines(path, left) || words(path, wordPattern, left, right) || router(right, toDigesters)
    }
  }

  /**
    * Yield a process that repeatedly accepts a job then parses the file it denotes, routeing each
    * word of each job down the appopriate output stream to a digester. Words are determined by the
    * `wordpat` of the job which is a regular expression.
    *
    * @param me    reader's identity
    * @param jobs  the stream of jobs to be processed
    * @param toDigesters  the streams to which words will be written
    */
  def reader(me: Int, jobs: ?[Job], toDigesters: Seq[![(String, Location)]]) = proc(s"reader($me")
  { val size = toDigesters.size
    repeat {
             val job    = jobs ? ()

             val parse  = parser(job.path, job.wordpat, toDigesters)

             parse()
           }

    for (out<- toDigesters) out.closeOut()
    jobs.closeIn()
  }

  /**
    * Yield a process that reads words and their occurences from `words` and collects them (with all their occurrences)
    * in the `digest`.
    *
    * @param me digester's id
    * @param words stream of words
    * @param digest mapping from words to their occurrences
    *
    */
  def digester(me: Int, words: ?[(String, Location)], digest: scala.collection.mutable.Map[String, List[Location]]) = proc(s"digester($me")
  {
    repeat {
             val (word, loc) = words?()
             digest.put(word, loc::digest.getOrElse(word, Nil))
             ()
           }
  }

  /**
    *  Ordering which places each lowercase letter just after its corresponding uppercase letter.
    *  Assumption: each Char of any Unicode plane can be represented in no more than 30 bits. In
    *  fact the Unicode code points are in the range `0x000000` to `0x10FFFF` and can be represented
    *  with 24 bits.
    */
  object PhoneBookOrder extends scala.math.Ordering[String]
  { /** Yield collation code of `c` */
    @inline def collate(c: Char): Int = if (c.isUpper)  (c.toLower<<1)  else  1 + (c<<1)

    /** Lexicographic comparison of strings by the collation codes of their characters */
    def compare(x: String, y: String): Int =
    { var diff = 0
      var i    = 0
      val xs = x.size
      val ys = y.size
      val cs = if (xs<=ys) xs else ys
      while (i<cs && x(i)==y(i)) i += 1 // scan common prefix
      // diff==0 && (i==cs || x(i)!=y(i))
      while (i<cs && diff==0)
      { diff = collate(x(i))-collate(y(i))
        i += 1
      }
      // i==cs || diff!=0
      if (diff==0) xs-ys else diff
    }
  }

}



