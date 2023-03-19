package io.threadcso.process

trait ProcessExports {

  type Stopped = io.threadcso.process.Stopped

  /** A process that does nothing. */
  val SKIP = Process.SKIP

  /** Terminate the current process or the current `repeat` */
  @inline def stop = throw Process.stopped

  /** Process type */
  type PROC = io.threadcso.process.PROC

  /** Run the given `proc` in the current thread. */
  @inline def run(proc: PROC): Unit = proc()

  /** Run the given `proc` in an acquired thread, and return a handle on the
    * running thread.
    */
  @inline def fork(proc: PROC): Process.Handle = proc.fork

  /** {{{
    * proc ("name") { body }
    * }}}
    * is a `PROC`-valued expression denoting a simple process with the given
    * name.
    */
  @inline def proc(name: String)(body: => Unit): PROC =
    new Process.Simple(() => body).withName(name)

  /** {{{
    * proc { body }
    * }}}
    * is a `PROC`-valued expression denoting a simple process with a made-up
    * name.
    *
    * It is, in-effect, a procedure of type `()=>UNIT`.
    * {{{
    * run(proc { body }) = (proc { body })() = (()=>body)()
    * }}}
    */
  @inline def proc(body: => Unit): PROC =
    new Process.Simple(() => body).withName(Process.genName)

  /** Same as `proc(body)` */
  @inline def π(body: => Unit): PROC =
    new Process.Simple(() => body).withName(Process.genName)

  /** Same as `proc(name)(body)` */
  @inline def π(name: String)(body: => Unit): PROC =
    new Process.Simple(() => body).withName(name)

  /** Run the parallel composition of `procs` */
  @inline def par(procs: collection.Seq[PROC]): Unit =
    if (procs.isEmpty) {} else
      new Process.Par(s"""par(${procs mkString ","})""")(procs)()

  /** Same as `par(range.map(gen))` */
  @inline def par[T](range: collection.Seq[T])(gen: T => PROC): Unit =
    par(range.map(gen))

  /** The parallel composition of `procs`; unless `procs` is empty, in which
    * case `SKIP`.
    */
  @inline def ||(procs: collection.Seq[PROC]): PROC =
    if (procs.isEmpty) SKIP
    else new Process.Par(s"""||(${procs mkString ","})""")(procs)

  /** Same as `||(range.map(gen))` */
  @inline def ||[T](range: collection.Seq[T])(gen: T => PROC): PROC =
    ||(range.map(gen))

  /** Close down the thread pools, and exit the program. */
  @inline def exit(): Unit = Process.exit()

  /** Close down the thread pools, and exit the program, yielding `code`. */
  @inline def exit(code: Int): Unit = Process.exit(code)

}
