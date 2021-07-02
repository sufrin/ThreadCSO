package io.threadcso.process

/** A process throws a `Stopped` or one of its subclass `Throwables`
    to signify normal termination.
*/
class Stopped() extends Throwable {}
 
