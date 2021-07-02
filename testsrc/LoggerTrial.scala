import io.threadcso.debug.Logger

/** Simple trial of elidable logging. Exected output is something like as follows
if the CSO Logger library is compiled with elision level smaller than FINEST.

{{{
Logging two non-elidable calls
Logging two elidable calls
Logger(LogTest, detail=-1, radius=200, spins=0) Log
00.000,005:: main#1@LoggerTrial.scala:27.15: First non-elidable
00.002,208:: main#1@LoggerTrial.scala:28.15: Second non-elidable
00.002,913:: main#1@LoggerTrial.scala:30.11: First elidable
00.003,360:: main#1@LoggerTrial.scala:31.11: Second elidable
}}}

The last two lines are omitted if the CSO Logger library was compiled with
-Xelide-below 350

  */
object LoggerTrial
{
  val logger = Logger("Logger Trial", 200, -1)

  def main(args: Array[String]) {
    println("Logging two non-elidable calls")
    logger.log(1, "First non-elidable" )
    logger.log(1, "Second non-elidable")
    println("Logging two elidable calls")
    logger(1, "First elidable")
    logger(1, "Second elidable")
    logger.printState()
  }
}

