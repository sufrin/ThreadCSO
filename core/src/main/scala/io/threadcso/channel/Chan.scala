package io.threadcso.channel

//import  io.threadcso._
import io.threadcso.basis._

trait Chan[T]
    extends InPort[T]
    with OutPort[T]
    with Named[Chan[T]]
    with io.threadcso.debug.REGISTRY.Debuggable {

  /** Signal that the channel is to be closed forthwith */
  def close(): Unit

  /** The channel has just changed its state in a way that will affect
    * `outPortState`
    */
  def outPortEvent(portState: PortState): Unit = {}

  /** The channel has just changed its state in a way that will affect
    * `inPortState`
    */
  def inPortEvent(portState: PortState): Unit = {}
}
