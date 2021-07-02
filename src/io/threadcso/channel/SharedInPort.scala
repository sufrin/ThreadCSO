package io.threadcso.channel

/** An input port that may be shared among several readers simultaneously. */
trait SharedInPort[+T]  extends InPort[T]  {}
