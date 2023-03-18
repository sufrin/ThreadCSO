package io.threadcso.lock

import io.threadcso.channel.{OneOne}
import io.threadcso.{proc, serve}
import io.threadcso.alternation.channel.{OneOne => AltOneOne}

import io.threadcso.semaphore.BooleanSemaphore

/** A `LogBarrier(n)` supports the ''repeated'' synchronization of `n`
  * processes, each with a distinct identity, `0<=id<n`.
  *
  * If `b` is such a barrier then `b.sync(id)` calls are stalled until all `n`
  * identities have been accounted for.
  *
  * When `n==1` then `b.sync(0)` returns immediately: this is so multi-worker
  * structures can be tested with only a single-worker, and may be helpful when
  * testing cellular automata.
  *
  * This implementation sets up a tree-shaped network of 2N semaphores, and each
  * `sync(id)` requires 2 communications between the process `id` and each of
  * its (up to) two children. Under favourable conditons some of the
  * communications can take place in parallel on separate processors.
  *
  * Empirical tests seem to demonstrate the greater efficiency of this over the
  * regular form of `Barrier`.
  */
abstract class LogBarrier[T](
    private val n: Int,
    private val e: T,
    private val op: (T, T) => T,
    val name: String = ""
) extends Barrier[T] {
  assert(n >= 1)

  /** thread signals parent that it's ready */
  private[this] val up, down = Array.fill(n)(OneOne[T]())
  private[this] val shutdownChan = AltOneOne[Unit]()

  @inline private[this] def left(id: Int): Int = 1 + 2 * id
  @inline private[this] def right(id: Int): Int = 2 + 2 * id

  private[this] var result = e

  def sync(id: Int, t: T): T = {
    val l = left(id)
    val r = right(id)

    var lt, rt = e

    // await both children
    if (l < n) lt = up(l) ? ()
    if (r < n) rt = up(r) ? ()
    // children ready
    var temp = op(t, op(lt, rt))
    if (id != 0) {
      up(id) ! temp;
      temp = down(id) ? ()
      // ready(id).release() // tell parent
      // go(id).acquire() // await parent's signal
    } else {
      result = temp
    }
    // notify and pass down the result to the children
    if (l < n) down(l) ! temp
    if (r < n) down(r) ! temp
    return temp
  }

  private def server = proc(name) {
    serve(
      shutdownChan =?=> { _ =>
        shutdownChan.close()
        up.foreach(_.close())
        down.foreach(_.close())
      }
    )
  }
  server.fork

  def shutdown = shutdownChan ! ()
}

object LogBarrier {
  def apply(n: Int, name: String = "") =
    new LogBarrier[Unit](n, (), (_: Unit, _: Unit) => (), name) {
      def sync(id: Int): Unit = sync(id, ())
    }
}
