package io.threadcso.process

import java.util.concurrent.CountDownLatch


object Process {
       
       private 
       val processCount    = new java.util.concurrent.atomic.AtomicLong

       /** Generate a new process name */       
       def genName: String =  "Proc-%d".format(processCount.getAndIncrement)

  /** This is invoked to handle exceptional non-`Stopped`
    * termination of a component of a running parallel composite
    * process. Usually its value is a procedure that
    * prints a stack backtrace on the console.
    */

      val handleThrowable: (String, Throwable) => Unit = {
        case (name: String, t: Throwable) =>
             Console synchronized {
                     Console.println(s"Process $name terminated by throwing $t")
                     t.printStackTrace()
             }
      }

       type ProcessBody = () => Unit
       
       private type Latch = CountDownLatch
       
       /** Thrown when `stop` is invoked. The stack backtrace
           is meaningless.
       */
       val stopped : Stopped = new Stopped()
       
       /** An elementary process */
       class Simple(body: ProcessBody) extends PROC
       { 
         /** Acquire a thread and start the body of this process running in it; then
             return a handle on the running thread.
         */
         def fork: Handle = 
         { val handle = new Handle(name, body, new CountDownLatch(1))
           handle.start()
           handle
         }
         
         /** Run the code of the `body` in the current thread. */
         def apply(): Unit = body()
         
         __stackSize = 0L
         __name = "<anonymous>"
       }
       
       /** Runnable this process does nothing.
           It is syntactically a (left) unit for `||`
       */
       val SKIP: PROC = new Process.Simple(()=>{})
       {  override def name: String = "SKIP"
          override def ||(other: PROC): PROC = other         
       }
       
       /** Semantic parallel composition of processes */
       class Par(_name: String)(procs: collection.Seq[PROC]) extends PROC
       { __name      = _name
         __stackSize = 0L
         
         /** Run each component process in a separate thread. The
             first runs in the current thread; the others are
             given threads of their own.
         */
         def apply(): Unit =
         { val latch       = new CountDownLatch(procs.size-1)
           val peerHandles = for (proc <- procs.tail) yield
                                 new Handle(proc.name, proc, latch, proc.stackSize)

           val firstHandle   = 
                             { val proc = procs.head
                               new Handle(proc.name, proc, null, proc.stackSize)
                             }
           
           for (handle<-peerHandles) { handle.start() } // start the peers
           firstHandle.run()                            // run in current thread
           // change name of current thread to reflect the fact that
           // the first component process has terminated and we are
           // waiting for the others all to terminate
           //if (latch.getCount>0) 
           //   Thread.currentThread.setName(s"${__name} (${procs(0).name} terminated)")
           // 
           latch.await()                                // await peers
           
           // ------------- Termination state --------------
           {
              var thrown = firstHandle.thrown
              for (handle<-peerHandles) 
              {   (thrown, handle.thrown) match 
                  { case (s:Stopped,   _:Stopped) => thrown = s
                    case (null,        s:Stopped) => thrown = s
                    case (s:Stopped,   null)      => thrown = s
                    case (null,        null)      => ()
                    case (_, _)                   =>
                         throw ParException(List(firstHandle.thrown) ++ peerHandles.map(_.thrown))
                  }
              }
              if (thrown!=null) throw thrown
           }           
         }
         
         def fork: Handle =
         { val handle = new Handle(name, apply _, new CountDownLatch(1))
           handle.start()
           handle
         }
         
       }
       
       /** A `ParException` is thrown when one or more components
           of a running parallel composition terminate with
           non-`Stopped` exceptions: `throwables(i)` is the
           `Throwable` thrown by the running `procs(i)` --
           or `null` when it terminated cleanly.
       */

 
       case class   ParException(throwables: collection.Seq[Throwable]) 
       extends Throwable 
       { override def toString: String = "ParException(%s)".format(throwables.mkString(", "))
       }
       
       
       /** A handle on a thread that will run a process. */     
       class Handle(name: String, body: ProcessBody, 
                    latch: Latch=null, stackSize: Long=0L) 
           extends java.lang.Runnable
       { /** The throwable, if any, thrown by a terminating process. */
         var thrown: Throwable = _
         /** The thread the handle is controlling; `null` if it hasn't been started. */
         var thread: Thread    = _
         
         override def toString = s"Handle($name, ..., $latch, $stackSize) thrown=$thrown; thread=$thread"
         
         /** Interrupt the thread the handle is controlling. */   
         def interrupt(): Unit =
         { 
           if (thread!=null) thread.interrupt()
         }    
         
         /** Acquire a thread (with the advisory `stackSize`) 
             and start `body` running in that thread.
         */
         def start(): Unit = executor.execute(this, stackSize)
         
         /**
             Wait for the forked process running with this handle to terminate
         */
         def join(): Unit = if (latch!=null) latch.await()
                  
         override def run(): Unit =
         { var originalName = ""
           try   
           { thread       = java.lang.Thread.currentThread
             originalName = thread.getName
             thread.setName(name)  
             body()
           } 
           catch 
           { case thrown: Stopped => this.thrown = thrown 
                   
             case other: Throwable  => 
               if (Process.handleThrowable != null) Process.handleThrowable(name, other)
               this.thrown = other
           }
           finally
           { thread.setName(originalName)            
             thread.setPriority(java.lang.Thread.NORM_PRIORITY)          
           }
                 
           if (latch!=null) latch.countDown()
         }
       }
       
       /** The standard executor used to run the threads in which processes execute. */
       private val executor: CSOExecutor = CSOThreads.executor
       
       def exit(): Unit = exit(0)
       def exit(code: Int): Unit = { executor.shutdown(); System.exit(code) }

}




























