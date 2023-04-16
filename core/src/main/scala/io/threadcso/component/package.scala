package io.threadcso
import io.SourceLocation._

/** <p> A collection of several process-generators that (mostly) yield processes
  * to work on (or produce) finite or infinite streams of values presented as
  * channels. All are designed to terminate cleanly -- 'i.e.' to `closeIn` or
  * `closeOut` all the channels that they communicate on in the appropriate
  * direction for the type of port.
  *
  * Some of these components were inspired by (or copied from) components from
  * the Plug'n'Play collection of JCSP (without necessarily retaining the P'n'P
  * names).
  *
  * {{{
  * @version 03.20120824
  * @author Bernard Sufrin, Oxford
  * }}}
  */
package object component {
  // import scala.language.postfixOps
  /** An implicit class that equips an `Iterable[T]` collection with methods
    * that write all its members to an output port.
    *
    * ''I confess that this class is here only because ScalaDoc will not
    * generate documentation for a channel unless it defines one or more
    * class-like entities.''
    */
  implicit class ExtendCollection[T](collection: Iterable[T]) {

    /** Write all the `collection` elements to `out`, then yield `true`; or
      * terminate early and yield `false` if `out` closes.
      */
    def writeTo(out: !![T]): Boolean =
      attempt { for (t <- collection) out ! t; true } { false }

    /** Write `Some(t)` to `out` for each `t` in `collection`, then write `None`
      * and yield `true`; or terminate early and yield `false` if `out` closes.
      */
    def writeSomeTo(out: !![Option[T]]): Boolean = {
      attempt {
        for (t <- collection) out ! Some(t)
        out ! None
        true
      } { false }
    }
  }

  /** Copy from the given input stream to the given output streams, performing
    * the outputs concurrently. Terminate when the input stream or any of the
    * output streams is closed.
    *
    * {{{
    *          in   /|----> x, ...
    * x, ... >---->{ | : outs
    *               \|----> x, ...
    * }}}
    *
    */
  def tee[T](in: channel.??[T], outs: channel.!![T]*): PROC = π("tee") {
    var v = null.asInstanceOf[T] // in.nothing
    val outputs = ||(for (out <- outs) yield π { out ! v })
    repeat { v = in ? (); run(outputs) }
    in.closeIn()
    for (out <- outs) out.closeOut()
  }

  /**
    * Merge several input streams into a single output stream. Terminate when
    * the output stream, or <i>all</i> the input streams have closed.
    *
    * {{{
    *   >---->|\  out
    * ins :   | }----->
    *   >---->|/
    * }}}
    *
    */
  def merge[T](ins: collection.Seq[??[T]], out: !![T]): PROC = π("merge") {
    serve(ins.head =?=> { x => out ! x })
    // serve ( | (for (in <- ins) yield in =?=> { x => out!x } ) )
    out.closeOut()
    for (in <- ins) in.closeIn()
  }

  /** Repeatedly input pairs of values `(l, r)` from `lin`, and `rin` and send
    * `f(l, r)` to `out`.
    */
  def zipwith[L, R, O](
      f: (L, R) => O
  )(lin: channel.??[L], rin: channel.??[R], out: channel.!![O]): PROC =
    π("zipWith") {
      var l = lin.nothing
      var r = rin.nothing
      val input = π { l = lin ? () } || π { r = rin ? () }

      repeat { input(); out ! f(l, r) }

      lin.closeIn(); rin.closeIn(); out.closeOut()
    }

  /** Turns a pair of streams into a stream of pairs.
    */
  def zip[L, R](
      lin: channel.??[L],
      rin: channel.??[R],
      out: channel.!![(L, R)]
  ): PROC =
    π("zip") {
      var l = lin.nothing
      var r = rin.nothing
      val doInputs = π { l = lin ? () } || π { r = rin ? () }

      repeat { doInputs(); out ! (l, r) }

      lin.closeIn()
      rin.closeIn()
      out.closeOut()
    }

  /** Output all the given `ts` onto the output port, then terminate.
    *
    * {{{
    * +------------+ t1, ...
    * | t1, ... tn +--------->
    * +------------+
    * }}}
    *
    */
  def const[T](ts: T*)(out: channel.!![T]): PROC = π("const") {
    for (t <- ts) out ! t; out.closeOut()
  }

  /**
    *  A composite component that sums its input stream onto its output stream.
    *  Try drawing it!
    */
  def Integrator(in: channel.??[Long], out: channel.!![Long]): PROC = {
    val mid, back, addl = OneOne[Long]
    (  zipwith((x: Long, y: Long) => x + y)(in, addl, mid)
    || tee(mid, out, back)
    || prefix(0L)(back, addl)
    )
  }

  /** Merges the streams `in` and `inj` onto `out`, giving priority to data
    * arriving on `inj`.
    */

  def inj[T](in: ??[T], inj: ??[T], out: !![T]): PROC = π("inj") {
    priserve(inj =?=> { x => out ! x } | in =?=> { x => out ! x })
    out.closeOut(); in.closeIn(); inj.closeIn()
  }

  /** Repeatedly reads pairs inputs from its two input channel.and outputs them
    * (in parallel, and ordered) to its two output channel.
    * {{{
    * x, ...--->[\/]---> max(x,y), ...
    * y, ...--->[/\]---> min(x,y), ...
    * }}}
    * Here is a four-channel sorting network composed of 5 such components.
    * {{{
    * -->[\/]--------->[\/]------------>
    * -->[/\]---+  +-->[/\]--+
    * |  |         |
    * |  |         +-->[\/]-->
    * -->[\/]------+         +-->[/\]-->
    * -->[/\]-+ |            |
    * | +---->[\/]---+
    * +------>[/\]------------->
    * }}}
    */
  def exchanger[T](
      l: channel.??[T],
      r: channel.??[T],
      lo: channel.!![T],
      hi: channel.!![T]
  )(implicit ev: T => Ordered[T]) /*[T <% Ordered[T]]*/
      : PROC =
    π("exchanger") {
      var lv, rv = l.nothing
      val rdBoth = π { lv = l ? () } || π { rv = r ? () }
      val wrBoth = π { lo ! rv } || π { hi ! lv }
      repeat {
        rdBoth()
        if (lv < rv) { val t = lv; lv = rv; rv = t }
        wrBoth()
      }
      l.closeIn(); r.closeIn()
      lo.closeOut(); hi.closeOut()
    }

  /** <p> Generate a `?[Unit]` to which an `()` is repeatedly written by a
    * server process `periodNS` nanoseconds after it is read.
    *
    * The server terminates at the next tick after the port is closed.
    * {{{
    * +----------+
    * | periodNS |>-------------> ()
    * +----------+
    * }}}
    */
  def Ticker(periodNS: Nanoseconds): ??[Unit] = {
    val ticks = OneOne[Unit]
    val Ping = () // avoid adaptation warning from scalac for ticks!()
    fork(π(s"Ticker($periodNS)") { repeat { ticks ! Ping; sleep(periodNS) } })
    return ticks
  }

  /** <p> Generate a `?[Nanoseconds]` on which the system nanosecond clock is
    * offered by a server process '''at least''' every `periodNS` nanoseconds.
    * The offer is withdrawn if the port is not read between the moment of the
    * offer and the moment at which the next offer is due. For example:
    * {{{
    *   Ticker(seconds(10))
    * }}}
    * yields a port that makes the absolute nanosecond clock available with a
    * resolution of 10 seconds.
    *
    * The server terminates at the next tick after the port is closed.
    * {{{
    * +----------+
    * | periodNS |>-------------> ... System.nanotime ...
    * +----------+
    * }}}
    */
  def Timer(
      periodNS: Nanoseconds,
      relativeTo: Nanoseconds = 0L
  ): ??[Nanoseconds] = {
    val out = OneOne[Long](s"Timer($periodNS)")
    val gen = π(s"Timer($periodNS)") {
      repeat {
        val now = System.nanoTime
        out.writeBefore(periodNS)(now - relativeTo)
        ()
      }
    }
    fork(gen)
    out
  }

  /** Drop the first value read from `in`, then copy values from `in` to `out`.
    */
  def tail[T](in: channel.??[T], out: channel.!![T]): PROC =
    π("tail") { in ? (); copy(in, out)() }

  /** Output the given `ts` to `out`, then copy values from `in` to `out`,
    * respecting the network-termination convention.
    */
  def prefix[T](ts: T*)(in: channel.??[T], out: channel.!![T]): PROC =
    π("prefix") { attempt { for (t <- ts) out ! t; copy(in, out)() } {} }

  /** Repeatedly copy values from `in` to `out`.
    */
  def copy[T](in: channel.??[T], out: channel.!![T]): PROC =
    π("copy") { repeat { out ! (in ? ()) }; out.closeOut(); in.closeIn() }

  /** Copy values from `in` to `out` that satisfy `pass`.
    */
  def filter[T](
      pass: T => Boolean
  )(in: channel.??[T], out: channel.!![T]): PROC =
    π("filter") {
      repeat { val v = in ? (); if (pass(v)) out ! v }
      out.closeOut()
      in.closeIn()
    }

  /** {{{
    * x, ... >-->[f]>-->f(x), ...
    * }}}
    */
  def map[I, O](f: I => O)(in: channel.??[I], out: channel.!![O]): PROC =
    π("map") {
      repeat { out ! f(in ? ()) }
      out.closeOut()
      in.closeIn()
    }

  /** Repeatedly write the string forms of values read from `in` onto the
    * standard output stream.
    */
  def console[T](in: channel.??[T]): PROC =
    π("console") { repeat { Console.println(in ? ()) } }

  /** Repeatedly output lines read from the given <tt>LineNumberReader</tt>. <p>
    * When `in` is associated with the user's terminal, rather than a file,
    * there is a potential race condition that the code here does not forbid
    * (explained below). Use `keyboard(out)` to play this role.
    *
    * <p> The race condition results in an unnecessary invocation of
    * <tt>in.readLine</tt> that will wait for input even though out is already
    * closed. <p> This happens when a program reading from the keyboard
    * terminates and closes channel ''downstream'' of the keyboard. This leaves
    * an unconsummated readLine waiting at the keyboard itself, that has to be
    * cleared by typing an end-line or EOF character. <p> The closing of out
    * should really abort an in.readLine that is already in progress. But that
    * would add unwarranted complexity to channel implementations.
    */
  def lines(in: java.io.LineNumberReader, out: channel.!![String]): PROC =
    proc("lines") {
      repeat {
        if (!out.canOutput) stop // nowhere to send anything we read.
        val line =
          try { in.readLine }
          catch { case _: Throwable => null }
        if (line == null) stop
        out ! line
      }
      out.closeOut()
      in.close()
    }

  /** Repeatedly output lines read from the given `Reader`.
    */
  def lines(in: java.io.Reader, out: channel.!![String]): PROC =
    lines(new java.io.LineNumberReader(in), out)

  /** Repeatedly output lines read from the given `File` */
  def lines(file: java.io.File, out: channel.!![String]): PROC =
    lines(new java.io.FileReader(file), out)

  /** Repeatedly output lines read from the file with the given path name
    */
  def lines(pathName: String, out: channel.!![String]): PROC =
    lines(new java.io.FileReader(new java.io.File(pathName)), out)

  /** Repeatedly output lines read from the standard input stream, stopping when
    * `out` either does not accept an output or can no longer accept an output.
    * Ending the standard input stream (Control-D to the terminal on Unix-based
    * operating systems) causes `out` to be closed for output, as does closing
    * the input port on the channel of which `out` is the output port.
    */
  def keyboard(out: channel.!![String]): PROC =
    proc("keyboard") {
      repeat(out.canOutput) {
        val ln = scala.io.StdIn.readLine()
        if (ln == null) stop
        out ! ln
      }
      out.closeOut()
    }

  /** Repeatedly output lines read from the standard input stream, stopping when
    * `out` either does not accept an output or can no longer accept an output;
    * and using the `prompt` thunk to generate a (new) prompt each time a line
    * is to be read. Ending the standard input stream (Control-D to the terminal
    * on Unix-based operating systems) causes `out` to be closed for output, as
    * does closing the input port on the channel of which `out` is the output
    * port.
    */
  def keyboard(out: channel.!![String], prompt: => String): PROC =
    proc("keyboard") {
      repeat(out.canOutput) {
        Console.print(prompt)
        val ln = scala.io.StdIn.readLine()
        if (ln == null) stop
        out ! ln
      }
      out.closeOut()
    }

  /** Read data from `in` as channel.as it appears there, copying the
    * most-recently read datum to `out` every `periodNS` nanoseconds.
    */

  def sampler[T](periodNS: Long, in: ??[T], out: !![T]): PROC =
    proc("sampler") {
      val ticker = Ticker(periodNS)
      var datum = in.nothing
      priserve(ticker =?=> { _ => out ! datum }
        | in =?=> (d => datum = d))
      in.closeIn(); out.closeOut(); ticker.closeIn()
    }
}
