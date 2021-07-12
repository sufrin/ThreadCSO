import io.threadcso._
import scala.language.postfixOps
/**
        Magic Square construction
        - Controller outputs candidate solutions to workers
        - Workers send completed solutions to an output stream
          and send all developments of each incomplete solution back to controller,
          signalling task completion along a separate channel. Here we
          send an integer (the number of the worker) rather than $()$
          down the \SCALA{taskFinished} channel.
[Not working: 11/1/2018 BS]
*/

object MagicSquares
{ var BUFSIZE  = 800
  var progress = false

  def Worker(me: Int,
             size:         Int, 
             tasksIn:      ?[PartialSoln], 
             tasksOut:     ![PartialSoln], 
             taskFinished: ![Int],                                      /**/
             solutions:    ![PartialSoln]) = proc(s"Worker $me") {
    var tasksRead, tasksDone, tasksFin = 0
    
   withDebuggerFormat(s"Worker $me: ($tasksRead/$tasksFin) found $tasksDone") {
    repeat  { 
        val partial = tasksIn?()
        // println(s"$me <-\n$partial ${partial.finished}")
        tasksRead += 1  
        if  (partial.finished) 
        {   tasksDone += 1
            solutions!partial
        }
        //  generate subsequent tasks
        // (ie those representing legal moves made onto a currently vacant square)
        else                        
        {  // choose a vacant square
           val (i,j) = partial.choose
           // println(s"$me -> $i,$j")
           for (k <- 1 to size*size)
               if (partial.isLegal(i,j,k)) tasksOut!partial.doMove(i,j,k)  /**/
        }
        tasksFin += 1
        taskFinished!me                                                    /**/
    }
    // a worker terminates when there are no more tasks to input
    // and closes its ''share'' of the solutions channel.
    solutions.closeOut
   }
  }

  // Controller
  def Controller(size:          Int, 
                 tasksOut:       ![PartialSoln],
                 tasksIn:        ?[PartialSoln], 
                 taskFinished:   ?[Int]) = proc("Controller") {
    
    // Initialise stack with an empty board
    val init  = new PartialSoln(size).empty
    val tasks = new scala.collection.mutable.Stack[PartialSoln]
    tasks.push(init)
    var busyWorkers = 0  // Number of workers currently busy
    var counter     = 0  // Number of tasks dispatched
    var lastFin     = -1 // last worker that finished
    
    withDebuggerFormat(s"Controller: $busyWorkers busy, $counter dispatched, ${tasks.size} pending. (Last $lastFin).") 
    {
      // Main loop
      serve (
         (busyWorkers>0 && taskFinished) =?=>                          /**/
                 { who  => busyWorkers -= 1; lastFin = who; /* println(s"$who finished") */ }

         | (!tasks.isEmpty && tasksOut) =!=>
              {  busyWorkers += 1
                 counter += 1
                 if (progress && counter%100==0) { print(s"$counter\r")  }
                 sleep(50*microSec)
                 tasks.pop
              }

        | (busyWorkers>0 && tasksIn)      =?=> 
              { ps   => tasks.push(ps) }


      )
    }
    // Finished, when tasks.isEmpty and busyWorkers==0
    tasksOut.closeOut
    tasksIn.closeIn
    taskFinished.closeIn
  }
  
  def Printer(solutions: ?[PartialSoln]) = proc("Printer") { 
    repeat{ val pb = solutions?; pb.printSolution } 
  }
  
  def Counter(solutions: ?[PartialSoln]) = proc("Counter") { 
    var count = 0;
    repeat { solutions?; count+=1; print("*") }
    print("%d solutions; ".format(count))
  }
  
  def Sink(solutions: ?[PartialSoln]) = proc("Sink") { 
    repeat{ val _ = solutions? }
  }


  // A solver is a network of workers generating to an output stream, and a controller
  
  def Solver(size: Int, workers: Int, solutions: ![PartialSoln]): PROC = {
    val fromW        = N2NBuf[PartialSoln](BUFSIZE, workers, 1, "fromW")       // workers-to-controller
    val toW          = N2N[PartialSoln](writers=1, readers=workers, "toW")     // controller-to-workers
    val taskFinished = N2N[Int](writers=workers, readers=1, "taskFinished")    // signal termination    /**/

    val Workers = || (for (w <- 0 until workers) yield Worker(w, size, toW, fromW, taskFinished, solutions))

    (  Workers 
    || Controller(size, toW, fromW, taskFinished)
    )
  }
  
  def time : Long = milliTime
  
  // Run it
  def main(args : Array[String]) {
    var print     = false
    var size      = 3
    var workers   = 3
    var benchmark = 0
    var samples   = 1
    for (arg <- args) 
        if (arg.matches("-d")) println(debugger) else
        if (arg.matches("-k=[0-9]+"))  BUFSIZE   = arg.drop(3).toInt else
        if (arg.matches("-w=[0-9]+"))  workers   = arg.drop(3).toInt else
        if (arg.matches("-p"))         print     = true else
        if (arg.matches("-P"))         progress  = true else
        if (arg.matches("-s=[0-9]+"))  size      = arg.drop(3).toInt else
        if (arg.matches("-x=[0-9]+"))  samples   = arg.drop(3).toInt else
        if (arg.matches("-b=[0-9]+"))  { benchmark = arg.drop(3).toInt; print=false } else
        {
          Console.println("Usage: MagicSquares [-x=samplessize(1) -b=benchmarksize(0)] [-w=workers(3)] [-p(false)] [-s=boardsize(3)]")
        }
         
    if (benchmark>0) {
       println("Size #W  ms")
       for (w <- 1 until benchmark+1) for (i<-0 until samples)
       { val t0 = time
         val solutions = N2NBuf[PartialSoln](BUFSIZE, writers=w, readers=1,  "Solutions")       // solutions channel
         (Solver(size, w, solutions) || Sink(solutions))()
         println("%4d %03d %3d".format(size, w, time-t0))
       }
    }
    else for (i<-0 until samples)
    { val t0 = time
      val solutions = N2NBuf[PartialSoln](BUFSIZE, workers, 1,  "Solutions")      // solutions channel
      (  Solver(size, workers, solutions)
      || (if (print) Printer(solutions) else Counter(solutions))
      )()
      println("%d ms".format(time-t0))
    }
    //exit
  }
}

