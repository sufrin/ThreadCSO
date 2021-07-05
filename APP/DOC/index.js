Index.PACKAGES = {"ox" : [], "ox.app" : [{"name" : "ox.app.App", "shortDescription" : "        An App is the core of a command-line application.", "object" : "ox\/app\/App$.html", "members_class" : [{"label" : "Usage", "tail" : "(): Unit", "member" : "ox.app.App.Usage", "link" : "ox\/app\/App.html#Usage():Unit", "kind" : "def"}, {"label" : "Invalid", "tail" : "(arg: String, remaining: List[String], args: List[String]): Unit", "member" : "ox.app.App.Invalid", "link" : "ox\/app\/App.html#Invalid(arg:String,remaining:List[String],args:List[String]):Unit", "kind" : "def"}, {"label" : "NotEnough", "tail" : "(opt: Opt, remaining: List[String], args: List[String]): Unit", "member" : "ox.app.App.NotEnough", "link" : "ox\/app\/App.html#NotEnough(opt:ox.app.App.Opt,remaining:List[String],args:List[String]):Unit", "kind" : "def"}, {"label" : "Unacceptable", "tail" : "(cmd: Opt, local: List[String], args: List[String]): Unit", "member" : "ox.app.App.Unacceptable", "link" : "ox\/app\/App.html#Unacceptable(cmd:ox.app.App.Opt,local:List[String],args:List[String]):Unit", "kind" : "def"}, {"label" : "Fail", "tail" : "(): Unit", "member" : "ox.app.App.Fail", "link" : "ox\/app\/App.html#Fail():Unit", "kind" : "def"}, {"label" : "main", "tail" : "(args: List[String]): Unit", "member" : "ox.app.App.main", "link" : "ox\/app\/App.html#main(args:List[String]):Unit", "kind" : "def"}, {"label" : "main", "tail" : "(args: Array[String]): Unit", "member" : "ox.app.App.main", "link" : "ox\/app\/App.html#main(args:Array[String]):Unit", "kind" : "def"}, {"member" : "ox.app.App#<init>", "error" : "unsupported entity"}, {"label" : "synchronized", "tail" : "(arg0: => T0): T0", "member" : "scala.AnyRef.synchronized", "link" : "ox\/app\/App.html#synchronized[T0](x$1:=>T0):T0", "kind" : "final def"}, {"label" : "##", "tail" : "(): Int", "member" : "scala.AnyRef.##", "link" : "ox\/app\/App.html###:Int", "kind" : "final def"}, {"label" : "!=", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.!=", "link" : "ox\/app\/App.html#!=(x$1:Any):Boolean", "kind" : "final def"}, {"label" : "==", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.==", "link" : "ox\/app\/App.html#==(x$1:Any):Boolean", "kind" : "final def"}, {"label" : "ne", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.ne", "link" : "ox\/app\/App.html#ne(x$1:AnyRef):Boolean", "kind" : "final def"}, {"label" : "eq", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.eq", "link" : "ox\/app\/App.html#eq(x$1:AnyRef):Boolean", "kind" : "final def"}, {"label" : "wait", "tail" : "(arg0: Long, arg1: Int): Unit", "member" : "scala.AnyRef.wait", "link" : "ox\/app\/App.html#wait(x$1:Long,x$2:Int):Unit", "kind" : "final def"}, {"label" : "wait", "tail" : "(arg0: Long): Unit", "member" : "scala.AnyRef.wait", "link" : "ox\/app\/App.html#wait(x$1:Long):Unit", "kind" : "final def"}, {"label" : "wait", "tail" : "(): Unit", "member" : "scala.AnyRef.wait", "link" : "ox\/app\/App.html#wait():Unit", "kind" : "final def"}, {"label" : "toString", "tail" : "(): String", "member" : "scala.AnyRef.toString", "link" : "ox\/app\/App.html#toString():String", "kind" : "def"}, {"label" : "notifyAll", "tail" : "(): Unit", "member" : "scala.AnyRef.notifyAll", "link" : "ox\/app\/App.html#notifyAll():Unit", "kind" : "final def"}, {"label" : "notify", "tail" : "(): Unit", "member" : "scala.AnyRef.notify", "link" : "ox\/app\/App.html#notify():Unit", "kind" : "final def"}, {"label" : "hashCode", "tail" : "(): Int", "member" : "scala.AnyRef.hashCode", "link" : "ox\/app\/App.html#hashCode():Int", "kind" : "def"}, {"label" : "getClass", "tail" : "(): Class[_ <: AnyRef]", "member" : "scala.AnyRef.getClass", "link" : "ox\/app\/App.html#getClass():Class[_]", "kind" : "final def"}, {"label" : "finalize", "tail" : "(): Unit", "member" : "scala.AnyRef.finalize", "link" : "ox\/app\/App.html#finalize():Unit", "kind" : "def"}, {"label" : "equals", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.equals", "link" : "ox\/app\/App.html#equals(x$1:Object):Boolean", "kind" : "def"}, {"label" : "clone", "tail" : "(): AnyRef", "member" : "scala.AnyRef.clone", "link" : "ox\/app\/App.html#clone():Object", "kind" : "def"}, {"label" : "asInstanceOf", "tail" : "(): T0", "member" : "scala.Any.asInstanceOf", "link" : "ox\/app\/App.html#asInstanceOf[T0]:T0", "kind" : "final def"}, {"label" : "isInstanceOf", "tail" : "(): Boolean", "member" : "scala.Any.isInstanceOf", "link" : "ox\/app\/App.html#isInstanceOf[T0]:Boolean", "kind" : "final def"}, {"label" : "Main", "tail" : "(): Unit", "member" : "ox.app.App.Main", "link" : "ox\/app\/App.html#Main():Unit", "kind" : "abstract def"}, {"label" : "Command", "tail" : ": String", "member" : "ox.app.App.Command", "link" : "ox\/app\/App.html#Command:String", "kind" : "abstract val"}, {"label" : "Options", "tail" : ": List[Opt]", "member" : "ox.app.App.Options", "link" : "ox\/app\/App.html#Options:List[ox.app.App.Opt]", "kind" : "abstract val"}], "members_object" : [{"label" : "Rest", "tail" : "(_pat: String, effect: (List[String]) => Unit, _help: String*): Rest", "member" : "ox.app.App.Rest", "link" : "ox\/app\/App$.html#Rest(_pat:String,effect:List[String]=>Unit,_help:String*):ox.app.App.Rest", "kind" : "def"}, {"label" : "Lit", "tail" : "(_pat: String, effect: (String) => Unit, _help: String*): Lit", "member" : "ox.app.App.Lit", "link" : "ox\/app\/App$.html#Lit(_pat:String,effect:String=>Unit,_help:String*):ox.app.App.Lit", "kind" : "def"}, {"label" : "Path", "tail" : "(_pat: String, effect: (String) => Unit, _help: String*): Path", "member" : "ox.app.App.Path", "link" : "ox\/app\/App$.html#Path(_pat:String,effect:String=>Unit,_help:String*):ox.app.App.Path", "kind" : "def"}, {"label" : "Real", "tail" : "(_pat: String, effect: (Double) => Unit, _help: String*): Real", "member" : "ox.app.App.Real", "link" : "ox\/app\/App$.html#Real(_pat:String,effect:Double=>Unit,_help:String*):ox.app.App.Real", "kind" : "def"}, {"label" : "Int64", "tail" : "(_pat: String, effect: (Long) => Unit, _help: String*): Int64", "member" : "ox.app.App.Int64", "link" : "ox\/app\/App$.html#Int64(_pat:String,effect:Long=>Unit,_help:String*):ox.app.App.Int64", "kind" : "def"}, {"label" : "Int32", "tail" : "(_pat: String, effect: (Int) => Unit, _help: String*): Int32", "member" : "ox.app.App.Int32", "link" : "ox\/app\/App$.html#Int32(_pat:String,effect:Int=>Unit,_help:String*):ox.app.App.Int32", "kind" : "def"}, {"label" : "PathArg", "tail" : "(_pat: String, effect: (String) => Unit, _help: String*): PathArg", "member" : "ox.app.App.PathArg", "link" : "ox\/app\/App$.html#PathArg(_pat:String,effect:String=>Unit,_help:String*):ox.app.App.PathArg", "kind" : "def"}, {"label" : "Arg", "tail" : "(_pat: String, effect: (String) => Unit, _help: String*): Arg", "member" : "ox.app.App.Arg", "link" : "ox\/app\/App$.html#Arg(_pat:String,effect:String=>Unit,_help:String*):ox.app.App.Arg", "kind" : "def"}, {"label" : "Flag", "tail" : "(_pat: String, effect: => Unit, _help: String*): Flag", "member" : "ox.app.App.Flag", "link" : "ox\/app\/App$.html#Flag(_pat:String,effect:=>Unit,_help:String*):ox.app.App.Flag", "kind" : "def"}, {"label" : "Quote", "tail" : "(s: String): String", "member" : "ox.app.App.Quote", "link" : "ox\/app\/App$.html#Quote(s:String):String", "kind" : "def"}, {"label" : "App", "tail" : "", "member" : "ox.app.App.App", "link" : "ox\/app\/App$.html#App=ox.app.App", "kind" : "type"}, {"label" : "OptFail", "tail" : "", "member" : "ox.app.App.OptFail", "link" : "ox\/app\/App$.html#OptFailextendsException", "kind" : "class"}, {"label" : "Rest", "tail" : "", "member" : "ox.app.App.Rest", "link" : "ox\/app\/App$.html#RestextendsApp.Opt", "kind" : "class"}, {"label" : "Lit", "tail" : "", "member" : "ox.app.App.Lit", "link" : "ox\/app\/App$.html#LitextendsApp.Opt", "kind" : "class"}, {"label" : "Path", "tail" : "", "member" : "ox.app.App.Path", "link" : "ox\/app\/App$.html#PathextendsApp.Opt", "kind" : "class"}, {"label" : "Int64", "tail" : "", "member" : "ox.app.App.Int64", "link" : "ox\/app\/App$.html#Int64extendsApp.Opt", "kind" : "class"}, {"label" : "Real", "tail" : "", "member" : "ox.app.App.Real", "link" : "ox\/app\/App$.html#RealextendsApp.Opt", "kind" : "class"}, {"label" : "Int32", "tail" : "", "member" : "ox.app.App.Int32", "link" : "ox\/app\/App$.html#Int32extendsApp.Opt", "kind" : "class"}, {"label" : "PathArg", "tail" : "", "member" : "ox.app.App.PathArg", "link" : "ox\/app\/App$.html#PathArgextendsApp.Opt", "kind" : "class"}, {"label" : "Arg", "tail" : "", "member" : "ox.app.App.Arg", "link" : "ox\/app\/App$.html#ArgextendsApp.Opt", "kind" : "class"}, {"label" : "Flag", "tail" : "", "member" : "ox.app.App.Flag", "link" : "ox\/app\/App$.html#FlagextendsApp.Opt", "kind" : "class"}, {"label" : "Opt", "tail" : "", "member" : "ox.app.App.Opt", "link" : "ox\/app\/App$.html#OptextendsAnyRef", "kind" : "abstract class"}, {"label" : "synchronized", "tail" : "(arg0: => T0): T0", "member" : "scala.AnyRef.synchronized", "link" : "ox\/app\/App$.html#synchronized[T0](x$1:=>T0):T0", "kind" : "final def"}, {"label" : "##", "tail" : "(): Int", "member" : "scala.AnyRef.##", "link" : "ox\/app\/App$.html###:Int", "kind" : "final def"}, {"label" : "!=", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.!=", "link" : "ox\/app\/App$.html#!=(x$1:Any):Boolean", "kind" : "final def"}, {"label" : "==", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.==", "link" : "ox\/app\/App$.html#==(x$1:Any):Boolean", "kind" : "final def"}, {"label" : "ne", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.ne", "link" : "ox\/app\/App$.html#ne(x$1:AnyRef):Boolean", "kind" : "final def"}, {"label" : "eq", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.eq", "link" : "ox\/app\/App$.html#eq(x$1:AnyRef):Boolean", "kind" : "final def"}, {"label" : "wait", "tail" : "(arg0: Long, arg1: Int): Unit", "member" : "scala.AnyRef.wait", "link" : "ox\/app\/App$.html#wait(x$1:Long,x$2:Int):Unit", "kind" : "final def"}, {"label" : "wait", "tail" : "(arg0: Long): Unit", "member" : "scala.AnyRef.wait", "link" : "ox\/app\/App$.html#wait(x$1:Long):Unit", "kind" : "final def"}, {"label" : "wait", "tail" : "(): Unit", "member" : "scala.AnyRef.wait", "link" : "ox\/app\/App$.html#wait():Unit", "kind" : "final def"}, {"label" : "toString", "tail" : "(): String", "member" : "scala.AnyRef.toString", "link" : "ox\/app\/App$.html#toString():String", "kind" : "def"}, {"label" : "notifyAll", "tail" : "(): Unit", "member" : "scala.AnyRef.notifyAll", "link" : "ox\/app\/App$.html#notifyAll():Unit", "kind" : "final def"}, {"label" : "notify", "tail" : "(): Unit", "member" : "scala.AnyRef.notify", "link" : "ox\/app\/App$.html#notify():Unit", "kind" : "final def"}, {"label" : "hashCode", "tail" : "(): Int", "member" : "scala.AnyRef.hashCode", "link" : "ox\/app\/App$.html#hashCode():Int", "kind" : "def"}, {"label" : "getClass", "tail" : "(): Class[_ <: AnyRef]", "member" : "scala.AnyRef.getClass", "link" : "ox\/app\/App$.html#getClass():Class[_]", "kind" : "final def"}, {"label" : "finalize", "tail" : "(): Unit", "member" : "scala.AnyRef.finalize", "link" : "ox\/app\/App$.html#finalize():Unit", "kind" : "def"}, {"label" : "equals", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.equals", "link" : "ox\/app\/App$.html#equals(x$1:Object):Boolean", "kind" : "def"}, {"label" : "clone", "tail" : "(): AnyRef", "member" : "scala.AnyRef.clone", "link" : "ox\/app\/App$.html#clone():Object", "kind" : "def"}, {"label" : "asInstanceOf", "tail" : "(): T0", "member" : "scala.Any.asInstanceOf", "link" : "ox\/app\/App$.html#asInstanceOf[T0]:T0", "kind" : "final def"}, {"label" : "isInstanceOf", "tail" : "(): Boolean", "member" : "scala.Any.isInstanceOf", "link" : "ox\/app\/App$.html#isInstanceOf[T0]:Boolean", "kind" : "final def"}], "class" : "ox\/app\/App.html", "kind" : "class"}, {"name" : "ox.app.OPT", "shortDescription" : " Type-directed invocation of the low-level 'ox.app.App' functions, using macros.", "object" : "ox\/app\/OPT$.html", "members_object" : [{"label" : "OPT", "tail" : "(tag: String, effect: (String) => Unit, help: String): Opt", "member" : "ox.app.OPT.OPT", "link" : "ox\/app\/OPT$.html#OPT(tag:String,effect:String=>Unit,help:String):ox.app.OPT.Opt", "kind" : "def"}, {"label" : "ELSE", "tail" : "(help1: String, effect: (String) => Unit, help: String): Lit", "member" : "ox.app.OPT.ELSE", "link" : "ox\/app\/OPT$.html#ELSE(help1:String,effect:String=>Unit,help:String):ox.app.App.Lit", "kind" : "def"}, {"label" : "REST", "tail" : "(tag: String, effect: (List[String]) => Unit, help: String): Opt", "member" : "ox.app.OPT.REST", "link" : "ox\/app\/OPT$.html#REST(tag:String,effect:List[String]=>Unit,help:String):ox.app.OPT.Opt", "kind" : "def"}, {"label" : "DoubleArg", "tail" : "(c: Context)(tag: scala.reflect.macros.blackbox.Context.Tree, flag: scala.reflect.macros.blackbox.Context.Tree, help: scala.reflect.macros.blackbox.Context.Tree): scala.reflect.macros.Universe.Tree", "member" : "ox.app.OPT.DoubleArg", "link" : "ox\/app\/OPT$.html#DoubleArg(c:scala.reflect.macros.blackbox.Context)(tag:c.Tree,flag:c.Tree,help:c.Tree):c.universe.Tree", "kind" : "def"}, {"label" : "OPT", "tail" : "(tag: String, flag: Double, help: String): Opt", "member" : "ox.app.OPT.OPT", "link" : "ox\/app\/OPT$.html#OPT(tag:String,flag:Double,help:String):ox.app.OPT.Opt", "kind" : "macro def"}, {"label" : "LongArg", "tail" : "(c: Context)(tag: scala.reflect.macros.blackbox.Context.Tree, flag: scala.reflect.macros.blackbox.Context.Tree, help: scala.reflect.macros.blackbox.Context.Tree): scala.reflect.macros.Universe.Tree", "member" : "ox.app.OPT.LongArg", "link" : "ox\/app\/OPT$.html#LongArg(c:scala.reflect.macros.blackbox.Context)(tag:c.Tree,flag:c.Tree,help:c.Tree):c.universe.Tree", "kind" : "def"}, {"label" : "OPT", "tail" : "(tag: String, flag: Long, help: String): Opt", "member" : "ox.app.OPT.OPT", "link" : "ox\/app\/OPT$.html#OPT(tag:String,flag:Long,help:String):ox.app.OPT.Opt", "kind" : "macro def"}, {"label" : "IntArg", "tail" : "(c: Context)(tag: scala.reflect.macros.blackbox.Context.Tree, flag: scala.reflect.macros.blackbox.Context.Tree, help: scala.reflect.macros.blackbox.Context.Tree): scala.reflect.macros.Universe.Tree", "member" : "ox.app.OPT.IntArg", "link" : "ox\/app\/OPT$.html#IntArg(c:scala.reflect.macros.blackbox.Context)(tag:c.Tree,flag:c.Tree,help:c.Tree):c.universe.Tree", "kind" : "def"}, {"label" : "OPT", "tail" : "(tag: String, flag: Int, help: String): Opt", "member" : "ox.app.OPT.OPT", "link" : "ox\/app\/OPT$.html#OPT(tag:String,flag:Int,help:String):ox.app.OPT.Opt", "kind" : "macro def"}, {"label" : "StringArg", "tail" : "(c: Context)(tag: scala.reflect.macros.blackbox.Context.Tree, flag: scala.reflect.macros.blackbox.Context.Tree, help: scala.reflect.macros.blackbox.Context.Tree): scala.reflect.macros.Universe.Tree", "member" : "ox.app.OPT.StringArg", "link" : "ox\/app\/OPT$.html#StringArg(c:scala.reflect.macros.blackbox.Context)(tag:c.Tree,flag:c.Tree,help:c.Tree):c.universe.Tree", "kind" : "def"}, {"label" : "OPT", "tail" : "(tag: String, flag: String, help: String): Opt", "member" : "ox.app.OPT.OPT", "link" : "ox\/app\/OPT$.html#OPT(tag:String,flag:String,help:String):ox.app.OPT.Opt", "kind" : "macro def"}, {"label" : "OPT", "tail" : "(tag: String, effect: => Unit, help: String): Opt", "member" : "ox.app.OPT.OPT", "link" : "ox\/app\/OPT$.html#OPT(tag:String,effect:=>Unit,help:String):ox.app.OPT.Opt", "kind" : "def"}, {"label" : "BoolArg", "tail" : "(c: Context)(tag: scala.reflect.macros.blackbox.Context.Tree, flag: scala.reflect.macros.blackbox.Context.Tree, value: scala.reflect.macros.blackbox.Context.Tree, help: scala.reflect.macros.blackbox.Context.Tree): scala.reflect.macros.Universe.Tree", "member" : "ox.app.OPT.BoolArg", "link" : "ox\/app\/OPT$.html#BoolArg(c:scala.reflect.macros.blackbox.Context)(tag:c.Tree,flag:c.Tree,value:c.Tree,help:c.Tree):c.universe.Tree", "kind" : "def"}, {"label" : "OPT", "tail" : "(tag: String, flag: Boolean, value: Boolean, help: String): Opt", "member" : "ox.app.OPT.OPT", "link" : "ox\/app\/OPT$.html#OPT(tag:String,flag:Boolean,value:Boolean,help:String):ox.app.OPT.Opt", "kind" : "macro def"}, {"label" : "BoolInv", "tail" : "(c: Context)(tag: scala.reflect.macros.blackbox.Context.Tree, flag: scala.reflect.macros.blackbox.Context.Tree, help: scala.reflect.macros.blackbox.Context.Tree): scala.reflect.macros.Universe.Tree", "member" : "ox.app.OPT.BoolInv", "link" : "ox\/app\/OPT$.html#BoolInv(c:scala.reflect.macros.blackbox.Context)(tag:c.Tree,flag:c.Tree,help:c.Tree):c.universe.Tree", "kind" : "def"}, {"label" : "OPT", "tail" : "(tag: String, flag: Boolean, help: String): Opt", "member" : "ox.app.OPT.OPT", "link" : "ox\/app\/OPT$.html#OPT(tag:String,flag:Boolean,help:String):ox.app.OPT.Opt", "kind" : "macro def"}, {"label" : "Opt", "tail" : "", "member" : "ox.app.OPT.Opt", "link" : "ox\/app\/OPT$.html#Opt=ox.app.App.Opt", "kind" : "type"}, {"label" : "App", "tail" : "", "member" : "ox.app.OPT.App", "link" : "ox\/app\/OPT$.html#App=ox.app.App", "kind" : "type"}, {"label" : "synchronized", "tail" : "(arg0: => T0): T0", "member" : "scala.AnyRef.synchronized", "link" : "ox\/app\/OPT$.html#synchronized[T0](x$1:=>T0):T0", "kind" : "final def"}, {"label" : "##", "tail" : "(): Int", "member" : "scala.AnyRef.##", "link" : "ox\/app\/OPT$.html###:Int", "kind" : "final def"}, {"label" : "!=", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.!=", "link" : "ox\/app\/OPT$.html#!=(x$1:Any):Boolean", "kind" : "final def"}, {"label" : "==", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.==", "link" : "ox\/app\/OPT$.html#==(x$1:Any):Boolean", "kind" : "final def"}, {"label" : "ne", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.ne", "link" : "ox\/app\/OPT$.html#ne(x$1:AnyRef):Boolean", "kind" : "final def"}, {"label" : "eq", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.eq", "link" : "ox\/app\/OPT$.html#eq(x$1:AnyRef):Boolean", "kind" : "final def"}, {"label" : "wait", "tail" : "(arg0: Long, arg1: Int): Unit", "member" : "scala.AnyRef.wait", "link" : "ox\/app\/OPT$.html#wait(x$1:Long,x$2:Int):Unit", "kind" : "final def"}, {"label" : "wait", "tail" : "(arg0: Long): Unit", "member" : "scala.AnyRef.wait", "link" : "ox\/app\/OPT$.html#wait(x$1:Long):Unit", "kind" : "final def"}, {"label" : "wait", "tail" : "(): Unit", "member" : "scala.AnyRef.wait", "link" : "ox\/app\/OPT$.html#wait():Unit", "kind" : "final def"}, {"label" : "toString", "tail" : "(): String", "member" : "scala.AnyRef.toString", "link" : "ox\/app\/OPT$.html#toString():String", "kind" : "def"}, {"label" : "notifyAll", "tail" : "(): Unit", "member" : "scala.AnyRef.notifyAll", "link" : "ox\/app\/OPT$.html#notifyAll():Unit", "kind" : "final def"}, {"label" : "notify", "tail" : "(): Unit", "member" : "scala.AnyRef.notify", "link" : "ox\/app\/OPT$.html#notify():Unit", "kind" : "final def"}, {"label" : "hashCode", "tail" : "(): Int", "member" : "scala.AnyRef.hashCode", "link" : "ox\/app\/OPT$.html#hashCode():Int", "kind" : "def"}, {"label" : "getClass", "tail" : "(): Class[_ <: AnyRef]", "member" : "scala.AnyRef.getClass", "link" : "ox\/app\/OPT$.html#getClass():Class[_]", "kind" : "final def"}, {"label" : "finalize", "tail" : "(): Unit", "member" : "scala.AnyRef.finalize", "link" : "ox\/app\/OPT$.html#finalize():Unit", "kind" : "def"}, {"label" : "equals", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.equals", "link" : "ox\/app\/OPT$.html#equals(x$1:Object):Boolean", "kind" : "def"}, {"label" : "clone", "tail" : "(): AnyRef", "member" : "scala.AnyRef.clone", "link" : "ox\/app\/OPT$.html#clone():Object", "kind" : "def"}, {"label" : "asInstanceOf", "tail" : "(): T0", "member" : "scala.Any.asInstanceOf", "link" : "ox\/app\/OPT$.html#asInstanceOf[T0]:T0", "kind" : "final def"}, {"label" : "isInstanceOf", "tail" : "(): Boolean", "member" : "scala.Any.isInstanceOf", "link" : "ox\/app\/OPT$.html#isInstanceOf[T0]:Boolean", "kind" : "final def"}], "kind" : "object"}]};