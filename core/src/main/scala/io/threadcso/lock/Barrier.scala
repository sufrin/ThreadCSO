package io.threadcso.lock

trait Barrier[T] {
  def sync(id: Int, value: T): T
}
