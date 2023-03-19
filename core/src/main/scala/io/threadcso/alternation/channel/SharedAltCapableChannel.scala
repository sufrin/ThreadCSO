package io.threadcso.alternation.channel

import io.threadcso.channel.{SharedOutPort, SharedInPort}

trait SharedAltCapableChannel[T]
    extends AltCapableChannel[T]
    with SharedOutPort[T]
    with SharedInPort[T] {}
