# ThreadCSO
## CSP/OCCAM-style channel-based concurrency

### NEWS: February 2023

With M Ahsan Al Mahir we have now made a version of ThreadCSO that can use
virtual threads (from **jdk20**) alongside platform threads.  Only minor
changes were necessary (the introduction of a new form of executor); and
99% of the API remains unchanged. 

These virtual threads provide dramatically enhanced possibilities for
channel-based programming since tens or hundreds of thousands of them can
be running in the same address space. Inter-process communication
performance (between processes running on virtual threads) appears
(in our tests) to better by orders of magnitude. 

To build this version


### Introduction

**ThreadCSO** is the second implementation of the *Communicating
Scala Objects* DSL (**CSO**) designed and built by Bernard Sufrin
at Oxford University in 2007 to support the *Concurrent Programming*
course that he taught there between 2004 and 2018, alternating every
few years with Gavin Lowe, who with Andrew Bate, made important
contributions to the DSL and to the course.

Though there are several more modern programming languages that
provide channel-based communication, few provide the low-overhead
compositional blend of **CSO** -- inspired by CSP/OCCAM -- that
includes (potentially-guarded and timed) input and output alternations,
"upstream" channel closing, straightforward clean network termination,
*etc*. 

**ThreadCSO** is published on GitHub now, with a few small example
programs, in the hope that it may prove useful to someone as a
pedagogical or a practical tool. The folder `Lectures` contains
an up-to-date introductory paper based on the 2007 paper, and
the slides for the (relevant) lectures given in 2018. Examples of larger
programs (will) appear elsewhere on GitHub.




### Processes and Process Notation 

*(Some extracts from the introductory paper)*

A CSO process is a value with Scala type `PROC` and is what an
experienced object oriented programmer would call a *stereotype*
for a thread. When a process is *started* any fresh threads that
are necessary for it to run are acquired from a pool; they are
returned to the pool when the process terminates. The present default
pool implementation acquires new worker threads from the underlying
JVM when necessary and ''retires'' threads that have remained dormant
in the pool for more than a certain period. Several varieties of
pool implementation are available: all can be specified and parameterized
at run-time.

Simple process expressions yield values of type `PROC` and
take one of the following forms

 1. `proc { `*expression* `}`
 2. `proc (`*name*`: String) {` *expression* `}`

If a *name* is not supplied, a new one is  automatically associated with the process value
as it is constructed. The *expression* must be of type `Unit`. These *name*s are useful
only in inspecting the output of a debugger.

Parallel compositions of `N` processes, each of type `PROC` take the form

 3. *p1* `||` *p2* `||` ... *pN* 
 
and the parallel composition of a finite *collection* of process values takes the
form

 4. `(||` *collection* `)` 
 
 This form is exactly equivalent to *p1* `||` *p2* `||` ... *pN* where the *pi* are
 the members (in some order) of the *collection*. An empty collection is equivalent
 to process that terminates immediately.
 
**Running**

If *p* is a process, then evaluation of the expression *p*`()` runs the process;
and the expression `run`*(p)* has exactly the same effect. 

The following cases are distinguished: 

 1. *p* is `proc{`*expr*`}`
 2. *p* is `proc(`*name*`){`*expr*`}` 
 
 Both forms cause *expr* to be evaluated in the current 
 thread -- *i.e.* the thread that invoked the run. The process terminates when
 the evaluation of *expr* terminates or throws an (uncaught) exception.  
 
 The behaviour of the expression *p*`()` cannot be distinguished
 from that of the expression `{`*expr*`}`, except that its *name*
 is used to identify the thread running *p* until *p* terminates.
 This identification can be helpful when inspecting a running CSO
 program using the CSO debugger.
 
 3. *p1* `||` *p2* `||` ... *pN* 
 
 Each of the processes except one is run in a new thread of its
 own; the remaining process is run in the current thread.  The
 process as a whole terminates only when *every* component  *pi*
 has terminated. But if one or more of the components terminated
 by throwing an uncaught exception then *when and only when they
 have all terminated* these exceptions are bundled into a `ParException`
 which is re-thrown, *unless they are all subtypes of*
 `io.threadcso.process.Stopped`; in which case a *single*
 `io.threadcso.process.Stopped` is thrown. (The reasons for this
 exception are explained in detail in the introductory paper. They
 are the foundation of the *network termination* protocol.)
 


**Forking**

It can sometimes simplify the construction of a large system to
have processes that run (more or less permanently) in the background.
The expression `fork(`*p*`)` runs *p*  in a new thread concurrent
with the thread that invoked `fork`, and returns a *handle* on the
running process that can be used (to some extent) to manipulate it.
The new thread is recycled when the process terminates.
       
### Ports and Channels

A CSO channel has two ports, one at each end, and in general is intended to transmit
(for reading) to its *input port* the data that is written to its *output port*. Ports
are parameterized by the type of data the channel transmits, and we define the abbreviations
`?[T]` and `![T]` respectively for
`InPort[T]` and `OutPort[T]`.

The most important method of an output port *op*`: ![T]` is its writing method *op*`!(`*value*`:T)`.

The most important methods of an input port *ip*`:?[T]` are its reading method *ip*`?():T`, and
its read-and-evaluate method *ip*`?{[U](`*f*`:T=>U):U`. If *f*`:T=>U`, the expression *ip*`?(`*f*`)`
has exactly the same effect as *f*`(`*ip*`?())`, namely to read a datum from *ip* 
(waiting, if necessary, for one to become available) then apply 
the function *f* to it.

The type `Chan[T]` is implemented by all channels that
carry values of type `T`; it is a subtype of both `![T]` and `?[T]`.
The implicit contract of every conventional `Chan` implementation
is that it transmits the data written at its output port to its input
port in the order in which the data is written.  

CSO provides a variety of channel implementations , with different
synchronization behaviours (synchronised, buffered, *etc*) and
different restrictions on the numbers of processes that may access
(*i.e.* use the principal methods of) their ports at any time
(many-to-many, one-to-one, *etc*). Channels may be closed by their
creator or from processes communicating using their ports: after
being closed they (eventually or immediately) cease to transmit
data.

A variety of other CSO constructs are designed to simplify the
programming of components / server processes that can be connected
in networks tjhat can straightforwardly be terminated. 
For example, a read from the input port of a channel
that is, or becomes, closed throws a `Stopped` exception, and such
exceptions *naturally* terminate **repeat** loops and propagate through
`||` compositions. 

For detailed descriptions of the varieties of channel, and of the
conventions for fuss-free clean network termination
see the introductory paper and Lectures/04....

As an example of the flavour of programming components in CSO, the
`tee` process generator below builds a process that, when started,
broadcasts data from its input port to all its output ports
concurrently: if the input port closes, or if any output port is
closed before or during a broadcast, then the process stops
broadcasting and closes all its ports, before itself terminating.

        def tee[T](in: ?[T], outs: Seq[![T]]) =
        proc { var data = in.nothing  // unspecified initial value
               val cast = || for (out<-outs) yield proc { out!data }
               repeat { data = in?(); cast() }
               for (out<-outs) out.closeOut; in.closeIn
             }

Any process that happens to be reading or writing
at the time will (if it is written using the termination conventions) 
itself do likewise, thereby propagating
termination across the communications network.

**Alternation**

It is often useful to be able to input from the first available channel of
a collection of channels: the **alt** construct supports this. For example,
a process generated by `tagger` (shown below) repeatedly inputs from one of 
two input ports, then outputs the value that was input "tagged" with either `0` or `1`
to indicate its origin:



        def tagger[T](l: ?[T], r: ?[T], out: ![(Int, T)]): PROC = 
        proc { repeat { alt ( l =?=> { vl => out!(0, vl) } 
                            | r =?=> { vr => out!(1, vr) } 
                            ) 
                      }
               l.closeIn; r.closeIn; out.closeOut 
             }
             
When either or both input ports can provide input, the **alt** chooses
(in principle nondeterministically) between those that can. The nondeterministic
choice is not necessarily made fairly, so one of the ports can "get ahead" of
the other in some circumstances. One way of making things fairer is to
program the repeated **alt** as a **serve** construct, thus:

        def tagger[T](l: ?[T], r: ?[T], out: ![(Int, T)]): PROC = 
        proc { serve ( l =?=> { vl => out!(0, vl) } 
                     | r =?=> { vr => out!(1, vr) } 
                     ) 
               l.closeIn; r.closeIn; out.closeOut 
             }
 
The **serve** is *almost* equivalent to the repeated **alt** except for using a "round-robin" 
policy of choosing between ports that are simultaneously ready in successive iterations.

Both these "taggers" obey the termination protocol. The (**repeat**ed)
**alt** fails with a `Stopped` exception, thereby terminating the
**repeat**, either if  `out` becomes closed, or *both* `l`, and `r`
become closed. The **serve** has identical termination behaviour.

Notice that the above alternations are composed of a sequence of **events** of
the form *inport*`=?=>`*function*. The *function* is invoked when the port *can* 
provide input and is *chosen* by the alternation to do so. Such events can also be guarded
with boolean guards, and a port is not considered by an alternation construct if
its guard is false. 

A good example of the use of guarded events, and a mix of input and
output events is the (purely pedagogic) example of a two-place buffer
implementation that appears in the chapter on alternation: Lectures/06.

        def Buff2Alt[T](in: ?[T], out: ![T]) = proc {
            var x     = in?()
            var empty = false
            serve ( (empty  && in)  =?=> { y => x=y; empty=false }
                  | (!empty && in)  =?=> { y => out!x; x=y } 
                  | (!empty && out) =!=> { empty=true; x }
                  )
            }

  * When `empty` its next action *must* be to input from `in`. 
  * When `!empty`, either:
    1. An input can be accepted, but then the current value of `x` must be output immediately
    2. `x` can be output if there is demand from (the other end of
    the channel connected to) `in`, and then it becomes `empty` again. 
    
    An output event consists of a possibly-guarded channel, followed
    by `=!=>`, followed by a block that is executed to provide the
    value to be output if the event is enabled and is chosen by the
    alternation. In the example we anticipate its becoming empty
    again before providing the value (`x`).

**The Dining Philosophers**

If you are here, then you will already know the (purely pedagogical) 
"Dining Philosophers" problem. For a complete example of the use of **CSO** here we
show a simulation of the  classical solution to avoiding deadlock, wherein the
majority of philosophers are left handed, but one is right handed.
Several other solutions will appear in due course.

        import io.threadcso._
        import scala.language.postfixOps
        
        object PhilsLeft 
        {
          // Philosophers' actions 
          abstract class Action {}
          case class Pick(who: Int)  extends Action
          case class Drop(who: Int)  extends Action
        
          val N = 5 // Number of philosophers
        
          val random = new scala.util.Random
        
          // Simulate basic actions
          def Eat   = sleep(50*milliSec)
          def Think = sleep(random.nextInt(80)*milliSec) 
          def Pause = sleep(50*milliSec)
        
          // buffered channel of indefinite capacity to report what's happening  
          val report = N2NBuf[String] (0, 0, 1, "report")
          
          // A single philosopher with identity `me`: 0 is right-handed
          def Phil(me: Int, left: ![Action], right: ![Action]) = proc("Phil"+me) {
            repeat {
              Think
              if (me==0) {
                 right!Pick(me); report!(me+" picks up right fork"); Pause
                 left!Pick(me);  report!(me+" picks up left fork");  Pause
              } else {
                 left!Pick(me);  report!(me+" picks up left fork");  Pause
                 right!Pick(me); report!(me+" picks up right fork"); Pause
              }
              report ! (me+" eats"); Eat
              left!Drop(me);  report!(me+" drops left fork"); Pause
              right!Drop(me); report!(me+" drops right fork"); Pause
            }
          }
        
        
          // A single fork
          def Fork(me: Int, left: ?[Action], right: ?[Action]) = proc("Fork"+me) {
            var owner: String="Nobody"
            // Associate this fork's state with a debugger format 
            // In case of deadlock the debugger can be invoked froma web browser, 
            // and the fork's current owner can be determined by inspection
            withDebuggerFormat(s"Fork ${me} with $owner")
            { // Forks must be dropped by the philosopher who picked them up
              serve
              {(  left  =?=> { case Pick(x) => owner=s"$x"; 
                                               left?{ case Drop(y) => assert(y==x); 
                                                      owner="nobody"} 
                             }
               |  right =?=> { case Pick(x) => owner=s"$x"; 
                                               right?{ case Drop(y) => assert(y==x); 
                                                       owner="nobody"} 
                             }
              )}
              println(s"Both the ports of FORK $me are closed: owned by ${owner}")
            }
          }
        
          // Copy messages from report onto the console
          val theConsole: PROC = proc ("Console") { repeat{ Console.println(report?()) } }
        
          // One to one channels to pick up and drop the forks:
          val philToLeftFork  = 
              for (i<-0 until N) yield 
                  OneOne[Action]  (s"Phil($i) to Fork($i)")
          val philToRightFork = 
              for (i<-0 until N) yield 
                  OneOne[Action]  (s"Phil($i) to Fork(${(N+i-1)%N})")
          // philToLeftFork(i)  is from Phil(i) to Fork(i);
          // philToRightFork(i) is from Phil(i) to Fork((i-1)%N)
        
        
          // Put the components together
          val AllPhils: PROC = || ( 
            for (i <- 0 until N) yield 
              Phil( i, philToLeftFork(i), philToRightFork(i) ) 
          )
        
          val AllForks: PROC = || ( 
            for (i <- 0 until N) yield 
              Fork( i, philToRightFork((i+1)%N), philToLeftFork(i) ) 
          )
                
          // And run the simulation
          def main(args : Array[String]) = 
          { println(debugger) // show the TCP port on which the debugger will report
            run(AllPhils || AllForks || theConsole)
          } 
        }


### EIEIO
In due course we will publish the **EIEIO**  (Extended Interface
to External I/O) library that provides implementations of cross-host
channels that can be used to build multi-host applications and
provides an (extensible) variety  of wire protocols.

*Bernard Sufrin, July 2021, and March 2023*










