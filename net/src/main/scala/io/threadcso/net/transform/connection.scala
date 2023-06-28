package io.threadcso.net.transform

import io.threadcso
import io.threadcso.transform.port
import io.threadcso.net.UDPTransport.{Datagram, Malformed, UDP}
import io.threadcso.net.transport.{Connection, NetConnection, SSLTransportInterface, TCPTransportInterface, UDPTransportInterface}

/**
  *  Tools to transform the traffic passing through connections by pre-output encoding
  *  and post-input decoding.
  *
  *  The tools `fromUDP` and `toUDP` make it straightforward to share client or server code between services
  *  provided over `TCP` (including `SSL`) transport and services provided over `UDP` transport.
  *
  *
  * Here's a (fabricated) example, in which a `[String,String]` connection is made to a server
  * either by `TCP` or by `UDP`, with the latter kind being transformed
  * into a `Datagram`-less one. The `interactWith` code is blind to the transport
  * used by the connection it is given.
  * {{{
  *  import java.net.{InetSocketAddress=>Address}
  *  def interactive(job: Settings): Unit = {
  *   val term = new TerminalConnection("quit")
  *   val wire = factory.StringTransportCRLF
  *   log.info(s"Testing: $job")
  *   val conn: Connection[String,String] =
  *   if (job.udp) {
  *     val conn =
  *        UDPConnection.connect(new Address(job.host, job.port),
  *                              wire, "UDP")
  *     fromUDP(conn)
  *   } else {
  *       TCPConnection.connected(new Address(job.host, job.port),
  *                               wire, "TCP")
  *   }
  *   interactWith(terminal, conn)
  *}
  * }}}
  */
object connection {
  /**
    *  Deliver a connection that `encode`s before outputting, and `decode`'s after inputting. Useful to
    *  transform `UDP` connections to `TCP` connections, and vice-versa.
    */
  def map[OUT, CODEDOUT, IN, CODEDIN](connection: Connection[CODEDOUT, CODEDIN])(encode: OUT=>CODEDOUT, decode: CODEDIN=>IN): Connection[OUT, IN] =
    new Connection[OUT, IN] {
      val out: threadcso.!![OUT] = port.map(connection.out) (encode)
      val in: threadcso.??[IN]   = port.map(connection.in)  (decode)
      def open(): Unit  = connection.open()
      def close(): Unit = connection.close()
      override def toString: String = s"Mapped($out, $in)"
    }

  /**
    * Transforms a UDP (datagram) connection into an ordinary connection by adding / stripping
    * datagram wrappers on output / input. The effect of this is to enable `UDP` connections to
    * be treated as if they are ordinary connections.
    *
    * This is at the cost of not knowing the source of a message on reading it,
    * nor being able to set the destination when writing it. But a `connect`ed UDP connection is already
    * committed to sending to a particular port and receiving from the same port, so there is
    * no need for either facility.
    *
    */
  def fromUDP[OUT, IN](connection: NetConnection[UDP[OUT], UDP[IN]]): Connection[OUT, IN] = {
    locally {
      assert(connection.transport.isInstanceOf[UDPTransportInterface], s"fromUDP requires a UDP Connection: $connection")
    }

    def decode(in: UDP[IN]): IN = in match {
      // TODO: think about how to treat a Malformed UDP. Should this just be handled as a one-time glitch
      //  in the
      case Datagram(in, _) => in
      case Malformed(_) => throw new IllegalArgumentException(s"fromUDP($connection).decode($in) -- nothing to decode")
    }

    def encode(out: OUT): UDP[OUT] = Datagram(out)

    map(connection)(encode, decode)
  }

  /**
    * Transforms a TCP/SSL connection to a simulated UDP connection (messages are
    * send and delivered as appropriately-attributed `Datagram`)
    */
  def toUDP[OUT, IN](connection: NetConnection[OUT, IN]): Connection[UDP[OUT], UDP[IN]] = {
    val from = connection.transport match {
      case tcp: TCPTransportInterface => tcp.getRemoteAddress
      case ssl: SSLTransportInterface => ssl.getRemoteAddress
      case other => throw new IllegalArgumentException(s"toUDP requires a TCP or an SSL connection: $other")
    }

    def decode(in: IN): UDP[IN] = in match {
      case in => Datagram(in, from)
    }

    def encode(out: UDP[OUT]): OUT = out match {
      case Datagram(in, _) => in
      case Malformed(_) => throw new IllegalArgumentException(s"toUDP($connection).encode($out) cannot handle $out")
    }

    map(connection)(encode, decode)
  }

}
