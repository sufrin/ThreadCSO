package io.threadcso.channel

/** An output port that may be shared among several writers simultaneously. */
trait SharedOutPort[-T] extends OutPort[T] {}
