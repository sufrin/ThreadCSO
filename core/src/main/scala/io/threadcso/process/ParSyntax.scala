package io.threadcso.process

/** Syntactic composition of `PROC`s.
  *
  * Evaluating an expression of the form
  *
  * `p`,,1,, `||` `p`,,2,, `|| ... ||` `p`,,n,,
  *
  * yields a
  *
  * `ParSyntax(List(``p`,,n,,`, ...,` `p`,,1,,`))`
  *
  * This is done because `||` is parsed left-associatively by Scala, and it's
  * more efficient to cons its right argument to the the front of the
  * accumulated list than it is to append it at the end.
  *
  * The computation of its meaning is deferred until (the first time) it is run
  * (by forking or applying).
  *
  * At this point the process list is reversed and '''compiled''' just in time
  * by being embedded in a `Par`. The reversal is not semantically necessary,
  * but it helps to make a subsequent `ParException` more intelligible. More
  * importantly, it is consistent with the way in which a prefix `||` constructs
  * its `Par`.
  */
class ParSyntax(_procs: List[PROC]) extends PROC {
  private lazy val revprocs = procs.reverse
  private lazy val compiled = new Process.Par(name)(revprocs)
  private val procs = _procs

  /** Run the component processes; each in its own thread (the first process
    * runs in the current thread. Terminate when all processes have terminated:
    * with or without throwing an exception.
    *
    * If all processes terminated without throwing an exception, then terminate
    * cleanly.
    *
    * If all exceptions thrown are of the `Stopped` family, then a single
    * `Stopped` exception is thrown on termination.
    *
    * Otherwise terminate by throwing a `ParException` that embeds the sequence
    * of termination reasons of the component processes (with `null` for a clean
    * termination).
    */
  def apply(): Unit = compiled()

  /** Fork this process and return a handle on the thread in which it is
    * running.
    */
  def fork: Process.Handle = compiled.fork

  override def name: String = revprocs.map(_.name) mkString "||"

  /** Construct the parallel composition of `this` and `other` */
  def ||(other: ParSyntax): PROC = new ParSyntax(other.procs ++ _procs)

  /** Construct the parallel composition of `this` and `other` */
  override def ||(other: PROC): PROC = new ParSyntax(other :: _procs)

}
