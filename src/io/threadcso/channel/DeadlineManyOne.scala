package io.threadcso.channel

/** A ManyOne-like channel, including a writeBefore operation. */
class DeadlineManyOne[T](name: String = "") extends DeadlineOneOne[T](name){
  /** We use synchronized blocks on lock to ensure at most one thread is active
    * in the writing operations. */
  private[this] val lock = new AnyRef

  override def !(value: T) = lock.synchronized{ super.!(value) }

  override def writeBefore(nsWait: Long)(value: T): Boolean = lock.synchronized{
    super.writeBefore(nsWait)(value)
  }

}
 
