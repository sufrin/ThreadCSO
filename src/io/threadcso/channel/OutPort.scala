package io.threadcso.channel

/**
  * A channel is a sequential conduit for data that is
  * written to its `OutPort`, and subsequently read from
  * its `InPort`.
  *
  *
  * For most ''normal'' channels at any instant: the sequence of data
  * that has been read from the channel with `?` is a prefix of the
  * sequence of data written to the channel by `!`. ''Dropping'' channels
  * may drop data under conditions of congestion.
  */
trait OutPort[-T]
{

  /** Output `value` to the port's channel. */
  def !(value: T): Unit

  /** Output `value` to the port's channel before `nsWait` has elapsed, and
    * return true; or return false
    */
  def writeBefore(nsWait: Long)(value: T): Boolean

  /** Signal that no further values will be output from the invoking thread:
    * ''idempotent''
    */
  def closeOut(): Unit

  /** Returns `false` if no further output can be accepted by this port. Returning
    * `true` is not a guarantee that further output will be accepted.
    */
  def canOutput: Boolean

  /** Name of the channel this port can write to */
  def name: String

  /** Current state of this port: for alternation implementations  */
  def outPortState: PortState

}
