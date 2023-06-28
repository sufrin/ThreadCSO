package io.threadcso.net.local

import io.threadcso._
import io.threadcso.net.transport.Connection


/**
  *  A service to which clients ask to `connect`. A `connect` request returns a `ServerResponse`, which
  *  will (when not refused) embody a `ServiceConnection[REQ,REP]`. On this connection the client
  *  will send values of type `REQ` and receive replies of type `REP`.
  *
  *  The service must be `open()`ed before it can respond to `connect`, and may be `close()`d at
  *  any time; after which `connect` requests are erroneous.
  *
  *  Each successful connection by a client forks a {{{session(client: ServiceConnection[REP, REQ])}}}
  *  (at the service end). The service "sees" the client as a `Connection[REP,REQ]` from which it
  *  inputs `REQ`uests, and outputs `REP`lies.
  *
  *  The (optional) `backlog` is the length of the `threadcso.Buf[Connect]` buffer used to buffer connection requests.
  *  Once this buffer is full, connection requests are delayed in the client `PROC`ess; but unless
  *  session setup takes a long time this won't be problematic.
  *
  *  == Note ==
  *  The intention here is to be able to simulate (from within the currently-running JVM) network services. The
  *  structure of code that provides, or uses, a `Service` is likely to be similar to that of code that
  *  provides (or uses) the services via networked connections.
  */
abstract class Service[REQ,REP] (serviceName: String, backlog: Int=1)  {
  import Service._
  import io.threadcso.process.Process.Handle
  private def toServerChan[T](ssize: Int)(name: String):   Chan[T] = if (ssize<=0) OneOne(name) else OneOneBuf(ssize, name)
  private def fromServerChan[T](csize: Int)(name: String): Chan[T] = if (csize<=0) OneOne(name) else OneOneBuf(csize, name)
  private val connectionPort: Chan[Connect[REQ,REP]] = N2NBuf(backlog, -1, 1, s"Service($serviceName).connectionPort")

  /**
    *  Connect to this service, and return the client-side of a connection to a (newly-forked) service `session`
    *  or a reason for the connection failing. The service must have been `open()`ed. The `clientName` is
    *  an uninterpreted name chosen by the entity making the connection.
    *
    *  `reqSize` and `repSize` specify the sizes of the buffered `threadcso.Chan`nels set up
    *  between client and server during the session.
    */
  def connect(clientName: String, reqSize: Int = 1, repSize: Int = 1): ServerResponse[REQ, REP] = {
    attempt {
      val answer = OneOne[ServerResponse[REQ, REP]]
      connectionPort ! Connect(clientName, reqSize, repSize, answer)
      val response = answer ? ()
      answer.close()
      response
    } { Refused(if (connectionPort.canOutput) s"connect($clientName) refused" else s"Service($serviceName) not running") }
  }

  /**
    * The handle on the open service.
    */
  var runningService: io.threadcso.process.Process.Handle = null

  /** Start the service -- after which `connect` requests (usually) succeed. */
  def open(): Unit  = runningService = serve.fork

  /**
    * Stop the service -- after which `connect` requests (usually) fail.
    * Service sessions are not necessarily affected by the service stopping;
    * this is a matter for individual `session` specifications.
    */
  def close(): Unit = {
    connectionPort.close()
    if (runningService!=null) runningService.interrupt()
  }

  def session(client: ServiceConnection[REP, REQ]): Unit

  /**
    *   A process that (repeatedly) services   `Connect` requests from a client by setting up a connection between
    *   that client and a newly forked process running the  `session` procedure; then returning
    *   the client side of that connection on the `reply` channel. That channel can be read exactly once
    *   and must then be closed. The `connect` method deals with the details, and is the
    *   preferred way of establishing a connection.
    *
    *   The client sends  requests on its `out` port and receives responses on its `in` port.
    *   Dually, the server receives requests on its `in` port and sends replies on its `out` port.
    */
  val serve: PROC = proc (s"Service($serviceName)"){
    repeat {
      connectionPort ? {
        case Connect(clientName, reqSize, repSize, reply) =>
          val toServer   = toServerChan[REQ](reqSize)(s"To($serviceName)for($clientName)")
          val fromServer = fromServerChan[REP](repSize)(s"From($serviceName)for($clientName)")
          val clientSide = new ServiceConnection[REQ, REP](clientName=clientName, serviceName = serviceName) {
            override def toString: String = s"clientSide: $out => $in"
            val out: !![REQ] = toServer
            val in:  ??[REP] = fromServer
            def open(): Unit = ()
            def close(): Unit = {
              toServer.close()
              fromServer.close()
            }
          }

          var sessionHandle: Handle = null

          val serverSide = new ServiceConnection[REP, REQ](clientName=clientName, serviceName = serviceName) {
            override def toString: String = s"serverSide: $out => $in"
            val out: !![REP] = fromServer
            val in:  ??[REQ] = toServer
            def open(): Unit = ()
            def close(): Unit = {
              toServer.close()
              fromServer.close()
              if (sessionHandle != null) sessionHandle.interrupt()
            }
          }
          sessionHandle = fork(proc(s"Service($serviceName) session for client ($clientName)") { session(serverSide) })
          reply ! Connected(clientSide)
      }
    }
  }
}

/** Companion object */
object Service {
  // Sending the service a `Connect` results in a response delivered via `reply`
  case class Connect[REQ,REP](name: String, reqSize: Int = 1, repSize: Int = 1, reply: !![ServerResponse[REQ, REP]])
  trait ServerResponse[REQ, REP]
  case class Connected[REQ,REP](server: ServiceConnection[REQ, REP]) extends ServerResponse[REQ, REP]
  case class Refused[REQ,REP](reason: String) extends ServerResponse[REQ, REP]

  /**
    *  Return a `Service[REQ,REP]` that services connection requests by forking the "server-side" `_session: ServiceConnection[REP, REQ]` process
    *  to respond to `REQ`uests by sending `REP`lies.
    *
    * @see Service
    */
  def apply[REQ,REP](serviceName: String, backlog: Int) (_session: ServiceConnection[REP, REQ] => Unit) : Service[REQ,REP] = new Service[REQ,REP](serviceName, backlog) {
    def session(client: ServiceConnection[REP, REQ]): Unit = _session(client)
  }
}

/**
  *   The client- or the server- end of a connection linking a client and service.
  *   At the client end, `OUT` is the type of requests, and `IN` the type of replies.
  *   At the service end, `IN` is the type of requests, and `OUT` the type of replies.
  */
abstract class ServiceConnection[OUT,IN](val clientName: String, val serviceName: String) extends Connection[OUT,IN]
