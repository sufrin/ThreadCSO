package io.threadcso

/**
  * Specifies primitive (non-alternation-capable) ports and channels; and implements
  * several channel types.
  */
package object channel
{
  type ?[+T] = InPort[T]
  type ![-T] = OutPort[T]
} 
