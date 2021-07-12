/**
  * Distriuted bag-of-tasks
  *
  * (termination protocol not quite right: BS, April 2013)
  */

package object DBOT {
 import io.threadcso._

 // def log(n: Int, s: String): Unit = Console.println(s)
  import io.threadcso.debug.Logging.log

 import scala.collection.mutable.Stack
 trait Task[T]
 { def finished: Boolean
   def subtasks: Iterable[T]
 }
 abstract class Message[T]
 case class Work[T](task: T) extends Message[T] 
 case class Terminating[T](definite: Boolean) extends Message[T]

  def Node[T <: Task[T]](me: Int, msgIn:  ?[Message[T]],
                                 msgOut: ![Message[T]], 
                                 out:    ![T],          
                                 first: T = null): PROC = 
 {  val B2W = OneOne[T](s"B$me to W$me")
    val W2B = OneOneBuf[Option[T]](0, s"W$me to B$me") // not quite unbounded
    val Bag: PROC = 
    proc(s"Bag$me") 
    { object TerminationState extends Enumeration 
      {  // The 3 possible states of master node (node 0) are
         //   RECOVERING  means recovering from an aborted termination
         //   RUNNING     means what it says
         //   TERMINATING means it has no work and nor does its predecessor in the ring
         // All other nodes are always in state NONMASTER
         val RUNNING, TERMINATING, RECOVERING, NONMASTER = Value
      }
      import TerminationState._
      var state = if (me==0) RUNNING else NONMASTER   
      
      // Local collection of tasks to be done
      // (in fact this should be a set; for otherwise it's possible for tasks to be duplicated)
      val work = new Stack[T]
      
      // Start with the first task, if given
      if (first!=null) work.push(first)
      
      // The bag records whether its worker is working
      var workerBusy = false
      val format = new DebuggerFormat(s"""Bag$me: $state $work $workerBusy""")
      format.register




      priserve
      {(  // busy worker, work to be done, successor solicits a task
        (workerBusy && !work.isEmpty && msgOut) =!=> { log(-1, s"Bag$me (busy) sends work to successor"); Work(work.pop) }

        // busy worker finishes a task or generates a new subtask
        |  (workerBusy && W2B) =?=>
           { case None          => log(-1, s"Bag$me receives None"); workerBusy = false
             case Some(subtask) => log(-1, s"Bag$me receives $subtask"); work.push(subtask)
           }

        // work to be done, (idle) worker solicits a task
        |   (!workerBusy && !work.isEmpty && B2W) =!=> { log(-1, s"Bag$me sends work to worker"); workerBusy=true; work.pop }

        // idle worker, work to be done, successor solicits work
        |   (!workerBusy && !work.isEmpty && msgOut) =!=> { log(-1, s"Bag$me (idle) sends work to successor"); Work(work.pop) }
        |   orelse ==>
            { Console.println(s"""Bag$me: $state $work $workerBusy""")
              if (!workerBusy && work.isEmpty)  // Node is idling
              {  if (state==RUNNING)            // Master node ...
                 { state=TERMINATING            // ... starts termination
                   msgOut!Terminating(false)    // ... tentatively
                   log(-1, s"""Bag$me: $state $work $workerBusy (sent Terminating(false))""")
                 }

                // Await a message from predecessor (there will certainly be one)
                val message = msgIn?()
                log(-1, s"""Bag$me: $state $work $workerBusy (received $message)""")
                message match
                { case Work(task) =>
                  workerBusy=true; B2W!task
                  log(-1, s"""Bag$me: $state $work $workerBusy (sent task to worker)""")
                  // abandon termination: jettison future Terminating messages
                  if (state==TERMINATING)
                  {
                    log(-1, s"""Bag$me: $state $work $workerBusy (started to RECOVER)""")
                    state = RECOVERING
                  }

                case Terminating(definite) =>
                  Console.println(s"Bag$me: Terminating($definite) $state $work $workerBusy")
                  state match
                  { case TERMINATING  =>   // convert tentative to definite
                     msgOut!Terminating(true)
                     log(-1, s"""Bag$me: $state $work $workerBusy (forwarded Terminating(true))""")
                     val confirm = msgIn?()        // await rearrival of termination
                     log(-1, s"""Bag$me: $state $work $workerBusy (received confirmation $confirm""")
                     out.closeOut    // close the output port
                     B2W.closeOut    // terminate the worker
                     log(-1, s"""Bag$me: $state $work $workerBusy (stopping)""")
                     stop            // terminate the serve loop
                    case RECOVERING   =>
                      state=RUNNING   // recovery is complete
                      log(-1, s"""Bag$me: $state $work $workerBusy (recovered)""")
                  case NONMASTER =>
                      msgOut!message        // pass on the termination message
                      if (definite)
                      { log(-1, s"""Bag$me: $state $work $workerBusy (terminating the worker)""")
                        B2W.closeOut    // terminate the worker
                        stop            // terminate serve loop if definite
                      }
                  }
                  // 899
                }
              }
            }

        )} // priserve
      B2W.closeOut
      }

    val Worker: PROC = proc(s"Worker$me")
    { repeat 
      { val task = B2W?()
        log(-1, s"""Worker$me: received $task""")
        if (task.finished)
           out!task                        // export finished  task
        else 
           for (st<-task.subtasks) 
               W2B!Some(st)                // send subtasks to my bag
        log(-1, s"""Worker$me: ready for more""")
        W2B!None                           // signal readiness for more
        log(-1, s"""Worker$me: asked for more""")
      }
      // W2B.closeOut
      log(-1, s"""Worker$me: terminated""")
    }
    (Bag || Worker)
 }
} // DBOT

