package ox.net

import app.OPT.{OPT, _}
import io.threadcso._
import ox.eieio.Factories.{crlfInFactory, crlfOutFactory}
import ox.logging.{Log, Logging => LOGGING}

abstract class NetChannelTest(doc: String) extends App {
  var factory: TypedChannelFactory[String, String] = UTF8ChannelFactory
  var host: String = "localhost"
  var port: Int    = 10000
  var debugPort    = 0

  val Command: String = doc
  val Options: List[Opt] = List(
    OPT("-crlf", { factory = CRLFChannelFactory }, "Use crlf string protocol"),
    OPT("-utf",  { factory = UTF8ChannelFactory }, "Use utf8 string protocol"),
    OPT("//.+:[0-9]+",   { case s"//$h:$p" => host = h; port=p.toInt; () }, "Set host and port"),
    OPT("//.+",          { case s"//$h" => host = h; () }, "Set host"),
    OPT("-d",            { debugPort = -1 }, "Disable debugger [enabled at random port otherwise]"),
    OPT("-d[0-9]+",      { case s"-d$p" => debugPort = p.toInt}, "Enable debugger on port."),
    OPT("-L[^=]+=[^=]+", { case s"-L${log}=${level}" => LOGGING.setLevel(log, level); () }, "Set log <name>=<level>"),
    OPT("-L=[^=]+",      { case s"-L=${level}" => LOGGING.setLevel("ALL", level); () }, "Set log ALL=<level>"),
  )

  def Main(): Unit = {
    if (debugPort > 0) System.setProperty("io.threadcso.debugger.port", debugPort.toString)
    if (debugPort >= 0) Console.println(debugger)
    Test()
  }

  def Test(): Unit

}

object connected extends NetChannelTest("connected") {
  def Test() = {
    val channel = NetChannel.connected(new java.net.InetSocketAddress(host, port), factory)

      val kbd = OneOne[String]("kbd")
      val fromHost = OneOneBuf[String](50, name = "fromHost") // A synchronized channel causes deadlock under load
      val toHost = OneOneBuf[String](50, name = "toHost") // A synchronized channel causes deadlock under load
      val log = Log("MAIN")

      // Bootstrap the channel processes
      val toNet        = channel.CopyToNet(toHost).fork
      val fromNet      = channel.CopyFromNet(fromHost).fork
      val fromKeyboard = component.keyboard(kbd, "").fork

      run(proc("ui") {
          repeat {
            kbd ? () match {
              case "" =>
                kbd.close()
                fromKeyboard.interrupt()
                stop
              case line =>
                log.fine(s"toHost ! $line")
                toHost ! line
            }
          }
          toHost.close()
          toNet.interrupt()
          fromNet.interrupt()
        }
          || proc("fromHost") {
          var n = 0
          repeat {
            n += 1
            val decoded = fromHost ? ()
            println(f"$n%-3d $decoded")
          }
          toHost.closeOut()
          kbd.close()
        }
      )
      kbd.close()
      fromHost.close()
      exit()
  }
}
