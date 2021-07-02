import ox.app.App._

import scala.collection.mutable
/**
    
    {{{ Bernard Sufrin, Oxford, 2015... $ }}}

   A simple example of command line parsing. This uses the low-level
   'ox.app.App' tools. A similar test program uses macros to make
   invocations of the low-level tools more concise.

   Our model for processing is that every path is processed in an
   environment: a pair (Env, Path) is called a Job.

   The environment is accumulated from the options
   given on the command line; and whenever a new path is added,
   the currently-prevailing environment is added to the Job queue.

  The Job queue is available when the options and paths
   have all been parsed -- but not before. An error in
   parsing causes the program to exit '''before any semantic processing
   has been done.'''
  */
//noinspection VarCouldBeVal,VarCouldBeVal
object AppTest extends App {

  import scala.collection.mutable.Queue
  case class Env 
  (
   var f: String,
   var g: Boolean,
   var h: Boolean,
   var i: Boolean,
   var k: Int,
   var r: Double
  ) 
  {
   override def toString = s"(f=$f g=$g h=$h i=$i k=$k r=$r)"
  }
  
  var env  = Env("Unset", false, false, false, 99, 0.0)
  var jobs = new mutable.Queue[(Env, String)]
  val Options = List (
     Flag("-help",    { Usage() },            "prints usage text")
   , PathArg("-f",    { arg => env.f=arg }, "«path» sets f to «path»")
   , Flag("-h",       { env.h = true },     "sets h")
   , Flag("-i",       { env.i = true},      "sets i" )
   , Flag("'+h",      { env.h = false},     "clears h")
   , Flag("'+i",      { env.i = false},     "clears i")
   , Int32("-k",      { arg => env.k=arg }, "«int» sets k")
   , Int32("--k=",    { arg => env.k=arg }, "«int» sets k")
   , Real("-r",       { _ => env.r }, "«real» sets r")
   , Path("[^-].*",   { f => jobs.enqueue((env.copy(), f)) },
                               "adds a path to the list to be processed",
                               "«path»")
   , Rest("--", (args =>
      for (f <- args) jobs.enqueue((env, f))),
          "interprets all subsequent arguments as paths")
   )
   val Command = "AppTest"
   
   def Main(): Unit =
       for ((env, path) <- jobs) Console.println(s"$path in $env")
     
}













