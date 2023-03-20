package io.threadcso.lock

import io.threadcso.semaphore.BooleanSemaphore
import io.threadcso.monitor.Monitor

/**

    Primitive high-performance synchronised channels with no debugger
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

     /** Synonym for read(): ?() == read() */
     @inline def ?(ignored : Unit): T = read()

     /** Synonym for f(read()) */
     @inline def ?[U](f: T=>U): U = f(read())
     
     /** Await state EMPTY then set state to FULL(T) */
     def write(t: T): Unit

     /** Synonym for write() */
     @inline def !(t: T): Unit = write(t)
    }

    /**
        An initially-EMPTY primitive.Chan[Unit]. Equivalent to
        a DataChan[Unit], but implemented more efficiently.
    */
    class UnitChan(name: String = null) extends Chan [Unit] {
      private val sema = BooleanSemaphore(false)
      @inline def read(): Unit   = sema.acquire()
      @inline def write(t: Unit) = sema.release()     
    }

    object UnitChan {
      @inline def apply(name: String = null): UnitChan =
        new UnitChan(name)
    }


    /**
        An initially-EMPTY primitive.Chan[T] implemented as a monitor.

        TODO: reimplement more efficiently using semaphores.
    */    
    class DataChan[T](name: String = null) extends Chan [T] {
      val monitor         = new Monitor(name)
      val isEmpty, isFull = monitor.newCondition

      // DTI: empty => buffer==null
      var buffer: T       = _
      var empty:  Boolean = true     
    
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
          empty  = false
          buffer = datum
          isFull.signal()
      }
      
    }

    object DataChan {
      @inline def apply[T](name: String = null): DataChan[T] =
        new DataChan[T](name)
    }

}
