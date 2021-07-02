package io.threadcso.semaphore.jdk

import io.threadcso.basis.{Identity, NameGenerator}
import io.threadcso.debug.REGISTRY._
import io.threadcso.semaphore.Semaphore
import java.util.concurrent.atomic.AtomicReference


/**
     A classical boolean semaphore that manages a single virtual ''permit''.
     Each `acquire` waits if necessary until the permit
     is available, and then makes it unavailable (its "owner" is said to be the
     process that last made it unavailable).  A `release`
     makes the permit available (if it wasn't already) to a waiting
     process.

     If the semaphore is `fair`  the semaphore will (under contention)
     guarantee first-in first-out acquisition of the permit.
     Fairness can impose very considerable delays.

     A thread that `release`s the permit ''need not have acquired it
     previously''.

     This implementation delegates most of its implementation to `java.util.concurrent.Semaphore`.

     A `BooleanSemaphore` can be registered with the debugger.

    @deprecated  Use [[io.threadcso.semaphore.CountingSemaphore]] instead unless you absolutely
                            need the fairness parameter.

  */
class BooleanSemaphore(available: Boolean, _name: String = null, fair: Boolean = false, _register: Boolean = false)
  extends
    CountingSemaphore(if (available) 1 else 0, BooleanSemaphore.genName(_name), fair, _register)
{
  private [this] val owner = new AtomicReference[Thread](if (available) null else Thread.currentThread())
  override def toString = s"""$name ${if (owner.get==null) "available" else s"owner: ${owner.get.identity}"} [waiting: ${getWaiting.map(_.identity).mkString(", ")}]"""

  override def acquire(): Unit = { super.acquire(); owner.set(Thread.currentThread()) }
  override def release(): Unit = if (owner.getAndSet(null)!=null) super.release()

  override def tryAcquire(wait: Long): Boolean = if (super.tryAcquire(wait)) { owner.set(Thread.currentThread()); true } else false

  //////// debugger interface

  override
  def showState(out: java.io.PrintWriter): Unit = out.print(s"BSEMA $this")

}


object BooleanSemaphore extends NameGenerator("BooleanSemaphore")
{
  def apply(available: Boolean,  name: String = null, fair: Boolean=false, register: Boolean=false): BooleanSemaphore =
    new BooleanSemaphore(available, name, fair, register)
}






















