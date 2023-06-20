import io.threadcso.net._
import io.threadcso.net.factory.ProtocolBufferChannel
import io.threadcso._
import io.threadcso.net.channels.Options.withOptions
import io.threadcso.net.channels.SocketOptions._

import example1.example1.Person
import example1.example1.PhoneNumber
import example1.example1.PhoneType

/**
 * A (very) cursory test of `ProtocolBufferChannel` that outputs `Person` messages to a server at
 * localhost:10000, and reads the same type of message from the server. Run it opposite the manual test
 * support program `io.threadcso.net.tests.reflect`.
 */
object ProtocolBufferTest {
  def main(args: Array[String]): Unit = {
      def me(n: Int, name: String): Person = {
                      Person(name   = name,
                      id     = n,
                      email  = Some("sufrin@sufrin.org"),
                      phones = List(PhoneNumber("07543-etc", Some(PhoneType.WORK))))
      }
      var n = 0
      println(debugger)

      val host = "localhost"
      val port = 10000

      val PCF = new ProtocolBufferChannel[Person,Person](Person)
                
      val netCon = withOptions(inBufSize=10*1024, outBufSize=10*1024, inChanSize=2) {
          TCPConnection.connected(new java.net.InetSocketAddress(host, port), PCF, "")
      }
      val kbdCon = new TerminalConnection(".\n")
      netCon.asTCP.setOption(TCP_NODELAY, true)
      netCon.open()
      kbdCon.open()
      val toKbd = π ("Kbd") {
          repeat {
             netCon.in ? {
                line => println(line)
             }
          }
          kbdCon.close()
      }

      val toNet = π ("Net") {
          repeat {
             kbdCon.in ? {
              case ".\n" => stop
              case name  => 
                   val p    = me(n, name)
                   n += 1
                   println(s"Sending: ${p.toProtoString}")
                   netCon.out ! p
            }
          }
          netCon.close()
      }
      (toKbd || toNet)()
      exit()
  }
}

 