package ox.app

import scala.reflect.macros.blackbox

/**
 <p>
 {{{ Bernard Sufrin, Oxford, 2015... $ }}}

 Type-directed invocation of the low-level 'ox.app.App' functions, using macros.

 In the following example our model for processing is
        that every path is processed in an environment: a
        pair `(Env, Path)` is called a `Job`.

 The environment is accumulated from the options
        given on the command line; and whenever a new path
        is added, the currently-prevailing environment is
        added to the `Job` queue.

 The `Job` queue is available when the options and
        paths have all been parsed -- but not before. An
        error in parsing causes the program to exit
        '''before any semantic processing has been done.'''

 {{{
object OptTest extends App
{
   import scala.collection.mutable.Queue
   case class Env (
    var f: String  = "Undefined",
    var g: Boolean = false,
    var h: Boolean = false,
    var i: Boolean = true,
    var k: Int     = 45,
    var r: Double  = 3.1415
   ) {
     override def toString = s"f=\$f, g=\$g, h=\$h, i=\$i, k=\$k, r=\$r"
   }

 var env  = new Env()
   var jobs = new Queue[(Env, String)]
   val Options = List (
      OPT("-help",  { Usage },                    "prints usage text")
    , OPT("-d",     { Console.println(Env()) },   "prints initial options")
    , OPT("-f",     env.f ,      "«path» sets f to «path»")
    , OPT("-g",     env.g ,      "inverts g")
    , OPT("-h",     env.h ,      "inverts h")
    , OPT("-i",     env.i ,      "inverts i")
    , OPT("-k",     env.k ,      "«int» sets k")
    , OPT("--k=",   env.k ,      "«int» sets k")
    , OPT("-r",     env.r,       "«real» sets r")
    , ELSE("«path>",   { f => jobs.enqueue((env.copy(), f)) },
                         "adds a path to the list to be processed")
    , REST("--", { case args => for (f <- args) jobs.enqueue((env, f)) },
                 "interprets all subsequent arguments as paths")
    )
    val Command = "OptTest"

 def Main =
        for ((env, path) <- jobs) Console.println(s"\${path} in \${env}")
 }
 }}}
*/
object OPT 
{  type App = ox.app.App
   type Opt = ox.app.App.Opt
   import scala.language.experimental.macros
   import scala.reflect.macros.blackbox.Context
   
   /**  An option that matches 'tag' inverts the value of 'flag' */
   def OPT(tag: String, flag: Boolean, help: String): Opt = macro BoolInv
   def BoolInv(c: blackbox.Context)(tag: c.Tree, flag: c.Tree, help: c.Tree): c.universe.Tree =
      { import c.universe._
        q"ox.app.App.Flag($tag, { $flag = ! $flag }, $help)"
      } 
   
   /** An option that matches 'tag' sets the value of 'flag' to 'value' */
   def OPT(tag: String, flag: Boolean, value: Boolean, help: String): Opt = macro BoolArg
   def BoolArg(c: blackbox.Context)(tag: c.Tree, flag: c.Tree, value: c.Tree, help: c.Tree): c.universe.Tree =
      { import c.universe._
        q"ox.app.App.Flag($tag, { $flag = $value }, $help)"
      } 
      
   /** An option that matches 'tag' invokes the effect */
   def OPT(tag: String, effect: => Unit, help: String): Opt =
       ox.app.App.Flag(tag, effect, help)
   
   /** An option that matches 'tag' sets 'flag' to the next argument */
   def OPT(tag: String, flag: String, help: String): Opt = macro StringArg
   def StringArg(c: blackbox.Context)(tag: c.Tree, flag: c.Tree, help: c.Tree): c.universe.Tree =
      { import c.universe._
        q"ox.app.App.Arg($tag, { case arg => $flag=arg }, $help)"
      } 
      
   /** An option that matches 'tag' sets 'flag' to the next argument.'toInt'*/
   def OPT(tag: String, flag: Int, help: String): Opt = macro IntArg
   def IntArg(c: blackbox.Context)(tag: c.Tree, flag: c.Tree, help: c.Tree): c.universe.Tree =
      { import c.universe._
        q"ox.app.App.Int32($tag, { case arg => $flag=arg }, $help)"
      }
       
   /** An option that matches 'tag' sets 'flag' to the next argument.toLong */
   def OPT(tag: String, flag: Long, help: String): Opt = macro LongArg
   def LongArg(c: blackbox.Context)(tag: c.Tree, flag: c.Tree, help: c.Tree): c.universe.Tree =
      { import c.universe._
        q"ox.app.App.Int64($tag, { case arg => $flag=arg }, $help)"
      } 
      
   /** An option that matches 'tag' sets 'flag' to the next argument.toDouble */   
   def OPT(tag: String, flag: Double, help: String): Opt = macro DoubleArg
   def DoubleArg(c: blackbox.Context)(tag: c.Tree, flag: c.Tree, help: c.Tree): c.universe.Tree =
      { import c.universe._
        q"ox.app.App.Real($tag, { case arg => $flag=arg }, $help)"
      } 
   
   /** An option that matches 'tag' invokes 'effect' on the remaining arguments */   
   def REST(tag: String, effect: List[String]=>Unit, help: String): Opt = 
       ox.app.App.Rest(tag, effect, help)
       
   /** An non-option argument (ie one that doesn't start with "-")
       invokes 'effect' on the following argument. The 'help1'
       text is used as a human-readable description of
       the non-option argument pattern. (I often use '"<path>"'
       here).
   */   
   def ELSE(help1: String, effect: String=>Unit, help: String) =
       new ox.app.App.Lit("[^-].*", effect, List(help, help1))
          
   /** If tag matches "-[-a-zA-Z0-9]+" an option that matches it 
       invokes 'effect' on the following argument. 
       
       Otherwise an option that matches it is passed directly to 'effect'.  
   */   
   def OPT(tag: String, effect: String=>Unit, help: String): Opt = 
       if (tag.matches("-[-a-zA-Z0-9]+")) 
          ox.app.App.Arg(tag, effect, help)
       else
          ox.app.App.Lit(tag, effect, help)
   
   
       
}







