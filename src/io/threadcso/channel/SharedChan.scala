package io.threadcso.channel

/**
  * A channel whose input and output ports may each be shared
  */


trait SharedChan[T] extends Chan[T] with SharedOutPort[T] with SharedInPort[T]


