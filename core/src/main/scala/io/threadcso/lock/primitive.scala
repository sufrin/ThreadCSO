package io.threadcso.lock

import io.threadcso.semaphore.BooleanSemaphore
import io.threadcso.monitor.Monitor

/**

    Primitive high-performance transport with no debugger
    interface or closing protocol; and no protection from inadvertent
    sharing.

    These are mainly intended to be used in the implementation of
    Barriers/CombiningBarriers -- to avoid the need for ad-hoc
    communication mediated by ad-hoc networks of semaphores.

    In what is perhaps a fit of self-indulgence, we provide
    the usual "standard" channel read and write operations.
*/

object primitive {

    /**
     *   A `primitive.Chan[T]` may be in one of the two conceptual
     *   states: EMPTY, or FULL(t:T)
     *   
     */
    trait Chan [T] {
    
     /** Await state FULL(t) then return t and set state to EMPTY */
     def read(): T

    /**
      * Returns `f(read())`, but synchronization (if any) happens
      * after the termination of `f`. This is known as an "extended rendezvous read".
      */
     def readThen[U](f: T => U): U = f(read())

     /** Synonym for read(): ?() == read() */
     @inline def ?(ignored : Unit): T = read()

     /** Synonym for f(read()) */
     @inline def ?[U](f: T=>U): U = f(read())

      /** Synonym for `readThen(f)` */
      @inline def ??[U](f: T => U): U = readThen(f)
     
     /** Await state EMPTY then set state to FULL(T) */
     def write(t: T): Unit

     /** Synonym for write() */
     @inline def !(t: T): Unit = write(t)

    }

    /** Synchronization mixin */
    trait Synchronization[T] extends Chan [T] {
      @inline def sync(): Unit = Barrier.sync()
      abstract override def read(): T         = try super.read() finally sync()
      abstract override def write(t: T): Unit = try super.write(t) finally sync()
      abstract override def readThen[U](f: T => U): U  = try f(super.read()) finally sync()

      /** A primitive (linear) barrier of size 2. @see io.threadcso.lock.Barrier */
      private object Barrier {
        var n = 2
        private[this] var waiting = 0 // number of processes currently waiting
        private[this] val wait =
          BooleanSemaphore(available = false, name = "Synchronization.wait")
        private[this] val enter =
          BooleanSemaphore(available = true, name = "Synchronization.enter")
        // enter is up iff a new batch of processes can enter the barrier
        // each entering process but the last is stalled by wait

        /** Wait until all `n` processes have called sync */
        def sync(): Unit = {
          enter.down()
          if (waiting == n - 1) // the last process arrives
            wait.up() // everyone can proceed (but cannot re-enter)
          else // a process arrives that isn't the last
          {
            waiting += 1
            enter.up()
            wait.down() // make it wait
            waiting -= 1
            if (waiting == 0)
              enter.up() // the last waiting process awoke
            else
              wait.up() // pass the baton to another waiter
          }
        }
      }
    }

    /**
        An initially-EMPTY primitive.Chan[Unit]. Equivalent to
        a DataChan[Unit], but implemented more efficiently.
    */
    class UnitChan(name: String = null) extends Chan[Unit] {
      override def toString: String = s"UnitChan($name)"
      private val sema = BooleanSemaphore(false, name, parent=this)
      @inline def read(): Unit   = sema.acquire()
      @inline def write(t: Unit) = sema.release()     
    }

    object UnitChan {
      @inline def apply(name: String = null, synchronized: Boolean = false): UnitChan =
        if (synchronized) new UnitChan(name) with Synchronization[Unit] else {
          new UnitChan(name)
        }
    }

  /**
    *  An initially-EMPTY `primitive.Chan[T]` implemented as a monitor.
    */
  class DataChanMonitor[T](name: String = null) extends Chan[T] {
      val monitor = new Monitor(name)
      val isEmpty, isFull = monitor.newCondition

      // DTI: empty => buffer==null
      var buffer: T = _
      var empty: Boolean = true

      def read(): T = monitor.withLock {
        while (empty) isFull.await(!empty)
        val res = buffer
        empty = true
        buffer = null.asInstanceOf[T]
        isEmpty.signal()
        res
      }

      def write(datum: T) = monitor.withLock {
        while (!empty) isEmpty.await(empty)
        empty = false
        buffer = datum
        isFull.signal()
      }
    }

  /**
    * An initially-EMPTY `primitive.Chan[T]` implemented with semaphores
    * subject to `Synchronization`.
    */
  class DataChan[T](name: String = null) extends Chan[T] {
    override def toString: String = s"DataChan($name)"
    val isFull  = new BooleanSemaphore(false, s"$name.isFull", false, this)
    val isEmpty = new BooleanSemaphore(true, s"$name.isEmpty", false, this)

    var buffer: T = _

    def read(): T = {
      isFull.acquire()
      val res = buffer
      buffer = null.asInstanceOf[T]
      isEmpty.release()
      res
    }

    def write(datum: T) = {
      isEmpty.acquire()
      buffer = datum
      isFull.release()
    }
  }

  object DataChan {
    @inline def apply[T](name: String = null, synchronized: Boolean = false): DataChan[T] =
      if (synchronized)
        new DataChan[T](name) with Synchronization[T]
      else
        new DataChan[T](name)
  }
}



