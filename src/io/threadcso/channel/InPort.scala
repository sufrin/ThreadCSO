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
  * may drop data under conditions of congestion: precise behaviours
  * of such channels should be documented with their definitions.
  */
trait InPort[+T] 
{

  /** Block until a value is available for input, then read and return it. */
  def ?() : T

  def ?(u: Unit): T = ?()

  /** Block until a value ''t'' is available for
    * input, then return `f(`''t''`)` -- equivalent to `f(?())`.
    */
  def ?[U](f: T => U): U

  /**
    *
    * Block until the earlier of the following events happens:
    *
    * - 1. A value, ''t'', is available for input.
    *
    * - 2. `ns` nanoseconds have elapsed.
    *
    * Return `Some(`''t''`)` in case 1, and `None` in case 2.
    *
    * The longest possible wait that can specified this way is about 146 years.
    */
  def readBefore(ns: Long): Option[T]

  /** Block until a value ''t'' is available for
    * input, then return `f(`''t''`)`. If the inport
    * is provided by a synchronized channel then synchronisation
    * with the termination of the sender's `!` is at the termination of
    * the computation of `f(`''t''`)` (this is sometimes
    * called an ''extended rendezvous'').
    */
  def ??[U](f: T => U): U

  /** Signal that no further attempts will be made to input from the invoking thread:
    * ''idempotent'' */
  def closeIn(): Unit

  /** Returns `false` if no further input will be supplied by this port. Returning
    * `true` is not a guarantee that further input will be supplied.
    */
  def canInput: Boolean


  /** The null value for this port -- equivalent
    * to `null.asInstanceOf[T]`. Useful to initialise variables
    * that will in due course be assigned values by reading from the port.
    */
  @inline def nothing: T = null.asInstanceOf[T]

  /** Name of the channel this port can read from */
  def name: String

  /** Current state of this port: for alternation implementations  */
  def inPortState: PortState
}
