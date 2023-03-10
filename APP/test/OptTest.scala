import app.OPT._

import scala.collection.mutable

/** <p> {{{Bernard Sufrin, Oxford, 2015... $}}}
  *
  * A simple example of command line parsing.
  *
  * Our model for processing is that every path is processed in an environment:
  * a pair `(Env, Path)` is called a Job.
  *
  * The environment is accumulated from the options given on the command line;
  * and whenever a new path is added, the currently-prevailing environment is
  * added to the Job queue.
  *
  * The Job queue is available when the options and paths have all been parsed
  * -- but not before. An error in parsing causes the program to exit '''before
  * any semantic processing has been done.'''
  */
//noinspection VarCouldBeVal,VarCouldBeVal
object OptTest extends App {

  import collection.mutable.Queue
  case class Env(
      var f: String = "Undefined",
      var g: Boolean = false,
      var h: Boolean = false,
      var i: Boolean = true,
      var k: Int = 45,
      var r: Double = 3.1415
  ) {
    override def toString = s"f=$f, g=$g, h=$h, i=$i, k=$k, r=$r"
  }

  var env = Env()
  var jobs = new mutable.Queue[(Env, String)]
  val Options = List(
    OPT("-help", { Usage() }, "prints usage text"),
    OPT("-d", { Console.println(Env()) }, "prints initial options"),
    OPT("-f", env.f, "<path> sets f to <path>"),
    OPT("-g", env.g, "inverts g"),
    OPT("-h", env.h, "inverts h"),
    OPT("-i", env.i, "inverts i"),
    OPT("-k", env.k, "<int> sets k"),
    OPT("--k=", env.k, "<int> sets k"),
    OPT("-r", env.r, "<real> sets r"),
    ELSE(
      "<path>",
      { f => jobs.enqueue((env.copy(), f)) },
      "adds a path to the list to be processed"
    ),
    REST(
      "--",
      (args => for (f <- args) jobs.enqueue((env, f))),
      "interprets all subsequent arguments as paths"
    )
  )
  val Command = "OptTest"

  def Main(): Unit =
    for ((env, path) <- jobs) Console.println(s"$path in $env")

}
