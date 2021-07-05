# ThreadCSO
## CSP/OCCAM-style channel-based concurrency


### Introduction

**ThreadCSO** is the second implementation of the *Communicating
Scala Objects* DSL designed and built by Bernard Sufrin at Oxford
University in 2007 to support the *Concurrent Programming* course
that he taught there, alternating every few years with Gavin Lowe,
who, with Andrew Bate, made important contributions to the DSL and
to the course.

Though there are several more modern programming languages that
provide channel-based communication, few provide a blend inspired
by CSP/OCCAM.

**ThreadCSO** is published on GitHub now, with a few small example
programs, in the hope that it may prove useful to someone as a
pedagogical or a practical tool. The folder `Lectures` contains
an up-to-date introductory paper based on the 2007 paper, and
the slides for the (relevant) lectures given in 2018. Examples of larger
programs (will) appear elsewhere on GitHub.

In due course we will publish the **EIEIO**  (Extended Interface
to External I/O) library that provides a implementations of cross-host
channels that can be used to build multi-host applications and
provides an (extensible) variety  of wire protocols.


### Processes and Process Notation 

*(Some extracts from the introductory paper)*

A CSO process is a value with Scala type `PROC` and is what an
experienced object oriented programmer would call a *stereotype*
for a thread. When a process is *started* any fresh threads that
are necessary for it to run are acquired from a pool; they are
returned to the pool when the process terminates. The present default
pool implementation acquires new worker threads from the underlying
JVM when necessary and ``retires'' threads that have remained dormant
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

Parallel compositions of `N` processes, each of type `PROC` takes the form

 3. *p1* `||` *p2* `||` ... *pN* 
 
and the parallel composition of a finite *collection* of process values takes the
form

 4. `(||` *collection* `)` 
 
 This form is exactly equivalent to *p1* `||` *p2* `||` ... *pN* where the *pi* are
 the members (in some order) of the *collection*.
 
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

The most important method of an output port *op*`:![T]` is its writing method *op*`!(`*value*`:T)`.

The most important methods of an input port *ip*`:?[T]` are its reading method *ip*`!():T`, and
its read-and-evaluate method *ip*`?{[U](`*f*`:T=>U):U`. If *f*`:T=>U`, the expression *ip*`?(`*f*`)`
has exactly the same effect as *f*`(`*ip*`?())`, namely to read a datum from *ip* 
(waiting, if necessary, for one to become available) then apply 
the function *f* to it.

The type `Chan[T]` is implemented by all channels that
carry values of type `T`; it is a subtype of both `![T]` and `?[T]`.
The implicit contract of every conventional `Chan` implementation
is that it transmits the data written at its output port to its input
port in the order in which the data is written.  

CSO provides a variety of channel implementations, with
different synchronization behaviours and different restrictions
on the numbers of processes that may access (*i.e.* use the
principal methods of) their ports at any time. Channels may be
closed by their creator or from processes communicating
using their ports: after being closed they
(eventually or immediately) cease to transmit data. 

For detailed descriptions of the varieties of channels, and of the
way closing can be used as part of a clean network termination
protocol see the introductory paper and Lectures/04....

A variety of other CSO constructs are designed to simplify the
programming of components / server processes that can be connected
in networks tjhat can straightforwardly be terminated. 
For example, a read from the input port of a channel
that is, or becomes, closed throws a `Stopped` exception, and such
exceptions *naturally* terminate **repeat** loops and propagate through
`||` compositions. 

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

*Bernard Sufrin, July 2021*










