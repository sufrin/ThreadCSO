package io.threadcso.lock

import io.threadcso.semaphore.BooleanSemaphore

/**
  * A boolean semaphore viewed as a non-reentrant lock implementation
  *
  * @param name    the name of the lock (for the debugger).
  * @param parent  the component (if any) in which the mutex will be used (for the debugger).
  */
class SimpleLock(name: String = null, parent: AnyRef = null)
  extends BooleanSemaphore(true, name, false, parent, 200)
          with io.threadcso.lock.Lock {
      def lock() = acquire()
      def unlock() = release()
}
