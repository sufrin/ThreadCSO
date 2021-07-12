import io.threadcso._
import scala.language.postfixOps

/**
        Magic Square construction
        - Controller outputs candidate solutions to workers
        - Workers send completed solutions to an output stream
          and send all developments of each incomplete solution back to controller,
          as well as signalling their readiness for further work, on the same channel.
    [Working: 11/1/2018 BS]

  */

object MagicSquaresOpt
{ var BUFSIZE = 800

  def Worker(me: Int,
             size:        Int, 
             tasksIn:     ?[PartialSoln], 
             tasksOut:    ![Option[PartialSoln]], 
             solutions:   ![PartialSoln]) = proc(s"Worker $me") {
   // For instrumentation/debugging purposes each worker keeps track of ...
   var tasksRead, tasksDone, tasksFin = 0
    
   withDebuggerFormat(s"Worker $me: ($tasksRead/$tasksFin) found $tasksDone")
   {  
    repeat  { 
        val partial = tasksIn?()
        tasksRead += 1          //**
        if  (partial.finished) 
        {   tasksDone += 1      //**
            solutions!partial
        }
        //  generate subsequent tasks
        // (ie those representing legal moves made onto a currently vacant square)
        else                        
        {  // choose a vacant square
           val (i,j) = partial.choose
           for (k <- 1 to size*size)
               if (partial.isLegal(i,j,k)) tasksOut!Some(partial.doMove(i,j,k))
        }
        tasksFin += 1           //**
        tasksOut!None       
    }
    // a worker stops outputting solutions when there are no more tasks to input
    // and closes its ''share'' of the solutions channel.
    solutions.closeOut
   }
  }

  // Controller
  def Controller(size:           Int, 
                 tasksOut:       ![PartialSoln],
                 tasksIn:        ?[Option[PartialSoln]]
                ) = proc("Controller") {
    
    // Initialise stack with an empty board
    val init  = new PartialSoln(size).empty
    var tasks   = new scala.collection.mutable.Stack[PartialSoln]
    tasks.push(init)

    
    var busyWorkers = 0 // Number of workers currently busy
    
    var counter     = 0 // ** Number of tasks dispatched: for instrumentation/debugger
    
   withDebuggerFormat(s"Controller: $busyWorkers busy, $counter dispatched, ${tasks.size} tasks pending")
   {
    // Main loop
    serve (true,
                (!tasks.isEmpty && tasksOut) =!=>
                      {  busyWorkers += 1
                         counter += 1
                         if (counter%5000==0) { print(".") }
                         tasks.pop
                      }

      | (busyWorkers>0 && tasksIn) =?=> 
            { case Some(ps) => tasks.push(ps)
              case None     => busyWorkers -= 1
            }
          )
   }
     
    // Finished when tasks.isEmpty and busyWorkers==0
    tasksOut.closeOut
    tasksIn.closeIn
  }
  
  def Printer(solutions: ?[PartialSoln]) = proc("Printer") { 
    var count = 0;
    repeat{ val pb = solutions?; pb.printSolution } 
  }
  
  def Counter(solutions: ?[PartialSoln]) = proc("Counter") { 
    var count = 0;
    repeat { solutions?; count+=1; print("*") }
    print("%d solutions; ".format(count))
  }
  
  def Sink(solutions: ?[PartialSoln]) = proc("Sink") { 
    repeat { val _ = solutions? }
  }


  // A solver is a network of workers generating to an output stream, and a controller
  
  def Solver(size: Int, workers: Int, solutions: ![PartialSoln]): PROC = {
    val fromW = N2NBuf[Option[PartialSoln]](BUFSIZE, workers, 1, "fromW")      // workers-to-controller
    val toW   = N2N[PartialSoln](writers=1, readers=workers, "toW") // controller-to-workers
 
    val Workers = || (for (w <- 0 until workers) yield Worker(w, size, toW, fromW, solutions))
 
    (  Workers 
    || Controller(size, toW, fromW)
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
        if (arg.matches("-k=-?[0-9]+"))  BUFSIZE   = arg.drop(3).toInt else
        if (arg.matches("-w=[0-9]+"))  workers   = arg.drop(3).toInt else
        if (arg.matches("-p"))         print     = true else
        if (arg.matches("-s=[0-9]+"))  size      = arg.drop(3).toInt else
        if (arg.matches("-x=[0-9]+"))  samples   = arg.drop(3).toInt else
        if (arg.matches("-b=[0-9]+"))  { benchmark = arg.drop(3).toInt; print=false } else
        {
          Console.println("Usage: MagicSquaresOpt [-x=samplesize(1) -b=benchmarksize(0)] [-w=workers(3)] [-p(false)] [-s=boardsize(3)]")
        }
         
    if (benchmark>0) {
       println("Size #W  ms")
       for (w <- 1 until benchmark+1) for (i<-0 until samples)
       { val t0 = time
         val solutions = N2NBuf[PartialSoln](3000, w, 1,  "Solutions")       // solutions channel
         (Solver(size, w, solutions) || Sink(solutions))()
         println("%4d %03d %3d".format(size, w, time-t0))
       }
    }
    else for (i<-0 until samples)
    { val t0 = time
      val solutions = N2NBuf[PartialSoln](3000, workers, 1,  "Solutions")      // solutions channel
      (  Solver(size, workers, solutions)
      || (if (print) Printer(solutions) else Counter(solutions))
      )()
      println("%d ms".format(time-t0))
    }
    exit
  }
}


