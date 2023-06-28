package io.threadcso.net.tests

import io.threadcso.{!!, ??, OneOneBuf, PROC, proc, run, sleep, ||, |}

/**
  *  Straightforward test of `Service` by running a few clients concurrently with
  *  a few services. Each client connects to a random proportion of the services; then
  *  repeatedly distributes inputs (acquired from the terminal) to it and echos
  *  its reply (if the reply arrives in time) or closes the connection (if not).
  */
object ServiceTest {

  import io.threadcso.net.TerminalConnection
  import io.threadcso.net.local._
  import Service._
  import io.threadcso.debug.DEBUGGER
  import io.threadcso.{repeat, seconds, serve}

  import math.random


  println(new DEBUGGER(9999))
  val log = ox.logging.Log("LOG")
  println(log)

  def pause(): Unit = sleep(seconds(random()))

  def trial(Services: Int, Clients: Int): Unit = {
    val terminal = new TerminalConnection("ctrl-D\n")
    terminal.open()

    /**
      *   Services transform their input slightly after a pause, and add a serial number to it.
      */
    val services: Seq[Service[String, (Int, String)]] =
      for {i <- 0 until Services} yield Service[String, (Int, String)](s"Service($i)", 1) {
        client =>
          log.info(s"Service($i) Session started for ${client.clientName}")
          var n = 0
          repeat {
            n += 1
            client.in ? {
              s => pause()
              client.out ! ((n, s"$i ==> $n: $s for ${client.clientName}"))
            }
          }
      }

    for { service <- services } service.open()

    /**
      *  Each client connects to about 3/4 of the services then repeatedly
      *  reads text from its source and sends it to each service for
      *  processing. Connections get closed if they respond too slowly,
      *  and the client terminates when it has no more open connections.
      */
    def clientProcess(client: Int, source: ??[String], out: !![String]): PROC = proc(s"Client($client)") {
      // connect to all services
      val connections = for { service <- services if random()>0.25 } yield {
        service.connect(s"Client $client", 4, 4) match {
          case Connected(connection) => connection
          case Refused(why) => throw new IllegalArgumentException(s"Service refused: $why")
        }
      }

      var openConnections = connections.length
      var slow = 0

      def transaction(text: String, connection: ServiceConnection[String, (Int, String)]): PROC = proc {
          if (connection.out.canOutput) {
            connection.out ! text
            connection.in.readBefore(seconds(0.85)) match {
              case Some(text) =>
                out ! s"\t$text\n"
              case None =>
                slow += 1
                if (slow>=2) {
                  log.warning(s"closing slow connection  ${connection.clientName} to ${connection.serviceName} ")
                  connection.close()
                  openConnections -= 1
                }
            }
          } else {
            log.info(s"Skipped slow connection  ${connection.clientName} to ${connection.serviceName} ")
          }
      }
      log.info(s"Client $client has $openConnections open connections")
      repeat (openConnections>0) {
        source ? {
          text => run(||( for { connection <- connections } yield transaction(text, connection)))
        }
      }
      log.info(s"Client($client) has no more open connections")
    }

    val terminalToClient =
        for { i <- 0 until Clients } yield OneOneBuf[String](2, s"Terminal to Client($i)")

    val clientToTerminal =
      for {i <- 0 until Clients} yield OneOneBuf[String](2, s"Terminal to Client($i)")

    val distribute = proc ("distribute") {
      repeat {
        terminal.in ? {
          case "ctrl-D\n" =>
            for { toClient <- terminalToClient } toClient.closeOut()
             // the closed connections terminate all existing sessions

          case line: String =>
             for { toClient <- terminalToClient } toClient ! line
        }
      }
    }

    /**
      *   Collect output from the clients, and pass it to the terminal.
      *   TODO: This wouldn't be necessary if the terminal output stream was shared.
      */
    val collect = proc("collect") {
      serve( | (for { chan <- clientToTerminal } yield chan =?=> { s => terminal.out ! s }))
    }

    run(collect || distribute || || (for { i <- 0 until Clients} yield clientProcess(i, terminalToClient(i), clientToTerminal(i))))

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
