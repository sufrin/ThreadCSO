package io.threadcso.channel

/** A channel that is guaranteed to be synchronous */
trait SyncChan[T] extends Chan[T] {}
