package io.threadcso.debug

import java.lang.ref.WeakReference

import scala.collection.concurrent.TrieMap
 
/**
        This object serves as a registry for (live) objects 
        that the debugger interface may be interested in.
*/
package object REGISTRY
{  /** Atomic reference to the key that will be used to enter the next
       registered object in the registry.
   */
   val  stateKey  = new java.util.concurrent.atomic.AtomicLong(0) 
   type StateKey = Long
   val  NONEWAITING: collection.Seq[Thread] = List()
   
   /**
        Objects that wish to make themselves known to the debugger
        interface should incorporate this trait, and `register` themselves
        at construction time. 
   */
   trait Debuggable
   {  /** This object's key in the registry (if non-negative) */
      var key: StateKey = -1
      /** Register this object */
      def register(): Unit   =
          if (key<0) key = io.threadcso.debug.REGISTRY.register(this)
      /** Unregister this object */
      def unregister(): Unit =
          if (key>=0) { registered -= key; key = -1 }
      /** Show the current state of this object: default uses `toString` */
      def showState(out: java.io.PrintWriter): Unit = out.print(this.toString): Unit
      /** Return a sequence of the threads currently waiting for this object,
          if any. 
          Default is the empty sequence.
      */
      def getWaiting: collection.Seq[Thread] = 
        io.threadcso.debug.REGISTRY.NONEWAITING
      
      /** Conditionally `register` this object with the debugger only for the duration 
          of the evaluation of `body`. To be used as a last resort for the exasperated 
          CSO toolkit debugger.
      */
      def withDebugger[T](condition: Boolean) (body: => T): T  =
      { if (condition)
        { 
           register()
           try
           { 
             body
           }
           finally
           { 
             unregister()
           }
        } 
        else
           body 
        
      }
      
      /** This object has a state worth showing right now: false if
          showState will do no output. 
      */
      def hasState: Boolean = true
   }
   
   val registered = new TrieMap[StateKey, WeakReference[Debuggable]]
   
   /** Register `obj` */
   def register(obj: Debuggable): StateKey =
   { val key = stateKey.getAndIncrement
     registered += ((key, new WeakReference(obj)))
     key
   }
   
   /** Return a mapping that maps each thread awaiting a registered object to
       the object it is awaiting. This mapping is used to interpret the
       states of waiting threads during a thread dump.

       @todo When the threadcso components that use `jdk.util.concurrent.LockSupport` are commissioned this
                  will become obsolete.
   */       
   def waiting: TrieMap[Thread, List[Debuggable]]=
   { val map = new TrieMap[Thread, List[Debuggable]]
     for ((_, sobj) <- registered)
     { val obj = sobj.get
       if (obj!=null)
          for (thread<-obj.getWaiting) 
              if (thread!=null) map += ((thread, obj :: map.getOrElse(thread, List())))
     }
     map
   }
   
   /** Utility to print captioned lists of (waiting) threads */
   def showThreads(
     out: java.io.PrintWriter, caption: String, threads: Seq[Thread]) =
   { import io.threadcso.basis._
     if (threads.nonEmpty)
     { var c=""
       out.print(caption)
       for (thread<-threads) { out.print(s"$c ${thread.identity}"); c="," }
     }
   }
  
}










