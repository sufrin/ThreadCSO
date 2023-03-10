package io.threadcso.alternation

/** Flags used to control the level of detail recorded when the execution of
  * alternations is logged.
  */

object LoggingFlags {

  /** Log start and end of registration: 1 */
  val REG: Int = 1 << 0

  /** Log unregistration: 2 */
  val UNREG: Int = 1 << 1

  /** Log selected event itself: 4 */
  val SEL: Int = 1 << 2

  /** Log when an alternation has to wait for an event: 8 */
  val WAIT: Int = 1 << 3

  /** Log after an alternation's wait for an event: 16 */
  val RUN: Int = 1 << 4

  /** Log when a port changes state between the alternation's prologue and
    * epilogue: 32
    */
  val NOTIFY: Int = 1 << 5

  /** Log termination of an alternation: 64 */
  val TERM: Int = 1 << 6

  /** Log change of state in a port: 128 */
  val PORTSTATE: Int = 1 << 7

  /** Log the registration/unregistration of a port with a running alternation:
    * 256
    */
  val PORTREG: Int = 1 << 8
}
