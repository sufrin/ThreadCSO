package io.threadcso.net.tests

class ServiceTest {

  import io.threadcso.{repeat, serve}
  import io.threadcso.net.TerminalConnection
  import io.threadcso.net.local._
  import Service._

  val terminal = new TerminalConnection("ctrl-D\n")
  terminal.open()

  import io.threadcso.debug.DEBUGGER

  println(new DEBUGGER(9999))
  val log = ox.logging.Log("LOG")
  println(log)

  def trial(Services: Int, Clients: Int): Unit = {

    val services =
      for {i <- 0 until Services} yield Service[String, (Int, String)](s"Service($i)", 1) {
        client =>
          log.info(s"Service($i) Session started for ${client.clientName}")
          var n = 0
          repeat {
            n += 1
            client.in ? {
              case s: String => client.out ! ((n, s"$i ==> $n: $s for ${client.clientName}"))
            }
          }
      }

    for {service <- services} service.open()

    val connections = for {service <- services; client <- 0 until Clients} yield {
      service.connect(s"Client $client", 4, 4) match {
        case Connected(connection) => connection
        case Refused(why) => throw new IllegalArgumentException(s"Service refused: $why")
      }
    }
    import io.threadcso.|

    serve( // send the line to each connection
      terminal.in =?=> { case "ctrl-D\n" =>
        for {connection <- connections} connection.close()
      // the closed connections terminate all existing sessions

      case line: String =>
        for {connection <- connections} connection.out ! line
      }
        // receive from any connection that's sending
        | (|(for {connection <- connections} yield
        connection.in =?=> { text => terminal.out ! s"\t$text\n" }))
    )
    // close all services
    for {service <- services} service.close()
    // close the terminal
    terminal.close()
    log.info("Terminated")
  }

  def main(args: Array[String]): Unit = {
    for {arg <- args} arg match {
      case s"$s.$c" => trial(Services = s.toInt, Clients = c.toInt)
    }
  }

}
