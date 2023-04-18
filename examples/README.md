# Some CSO examples

*The code provided here is compatible with Scala 2.13.  It will
compile with the most recently-published variant of the threadcso
library jar, and will run without change using **jdk 20**. Compiled
code will also run -- using Kernel threads rather than Virtual threads
by default -- using earlier versions of jdk.*

*Explanations of some of the principles behind the examples, and
of **threadcso** itself, can be found in the accompanying [lecture
notes](https://github.com/sufrin/ThreadCSO/Lectures).*


Bernard Sufrin, Oxford. 2007, 2011, 2017, 2023

## Solutions to Practicals

In this section we have collected solutions to practical problems
set as part of the Concurrent & Distributed Programming course at
the University of Oxford that was delivered either by Bernard Sufrin
or by Gavin Lowe between the years 2004 and 2022. 

The course content varied. Practicals were accompanied
by briefings and take up aspects of the course materials
from the year they were first set.


### Dining Philosophers

Except for `Phils.scala`, the `.scala` programs are his solutions
to a practical set often by Bernard Sufrin.  `Phils.scala` is a
deadlocking non-solution that illustrates the problem. It is very
easy to change the number of Philosphers (in the source text).


#### The Complete Practical Briefing

It is recommended that you read the material from the section of
the course notes on the Dining Philosophers before you attempt this
practical. The code from the lectures is on the course website. The
aim of the practical is to investigate some variants of the Dining
Philosophers, which aim to avoid deadlocks. You should implement
the first three variants below, and as many of the optional variants
you have the inclination to work on.

**Variant 1: a right-handed philosopher**

In the standard version of the dining philosophers, all the
philosophers are left-handed: they pick up their left fork first.
Implement a variant where one of the philosophers is right-handed,
i.e.she picks up her right fork first.

**Variant 2: using a butler**

Now consider a variant using an extra process, which represents a
butler. The butler makes sure that no more than four philosophers
are ever simultaneously seated.

**Variant 3: asking the forks using a protocol**

Now consider a variant where, if a philosopher is unable to obtain
her second fork, she puts down her first fork, and re-tries later.
This can be solved straighforwardly by implementing a bidirectional
protocol between forks and philosophers: a philosopher should be
able ask a fork to commit itself to her, and be answered positively
only if the fork has actually committed to her; negatively if the
fork has already committed to its other potential user. \[Hint: the
protocol will need two channels; it will simplify your program
structure to package them in a class.\]

**Optional, but desirable**

For each of the variants you implement include a way of counting
the number of times each philosopher has eaten, and of presenting
all this information periodically in addition to, or instead of,
the detailed reports of the actions of forks and philosophers. Think
of this as insurance against starvation! The same code should suffice
for all variants.

**Variant (optional: for fun): pile the forks in the middle of the
table**

In this variant, forks that are not being used sit in a pile in the
middle of the table, and philosophers can use any two of them to
eat.

**Variant (optional: timeouts)**

Now consider a variant where, if a philosopher is unable to obtain
her second fork, she puts down her first fork, and re-tries later.
Hint: This can be solved straighforwardly by using the deadlined
'writeBefore' method in the channel from a philosopher to one of
her forks. It may be worth doing some simple experiments in advance
to see how deadlined writes/reads interact with alternations. My
own solution used a deadline that was roughly twice the eating time.


**Reporting**

Your report should be in the form of a well-commented program, together
with a brief discussion of any design considerations and of your
results.



### Life (Conway's Game of Life)

The aim of this practical is to implement the Game of Life, a cellular
automaton invented by John Conway in 1970. The 
[Wikipedia entry on Life](http://en.wikipedia.org/wiki/Conway's_Game_of_Life)
describes it as follows:

“The universe of the Game of Life is an infinite two-dimensional
orthogonal grid of square cells, each of which is in one of two
possible states, live or dead. Every cell interacts with its eight
neighbours, which are the cells that are directly horizontally,
vertically, or diagonally adjacent. At each step in time the
following transitions occur:

   1.  Any live cell with fewer than two live neighbours dies, as if by
   needs caused by underpopulation.
   
   2.  Any live cell with more than three live neighbours dies, as if by
   overcrowding.
   
   3.  Any live cell with two or three live neighbours lives, unchanged,
   to the next generation.
   
   4.  Any tile with exactly three live neighbours cells will be
   populated with a living cell.

The initial pattern constitutes the ‘seed’ of the system. The first
generation is created by applying the above rules simultaneously to
every cell in the seed: births and deaths happen simultaneously,
and the discrete moment at which this happens is sometimes called a
tick. In other words, each generation is a pure function of the one
before. The rules continue to be applied repeatedly to create further
generations.”

You might want to read the rest of the Wikipedia article.

We will consider a variant with a finite rectangular grid, treated as a
toroid, i.e. where the top and bottom edges of the grid are treated as
being adjacent, as are the left and right edges.

#### Your task 

Your task is to implement the Game of Life. Your program should use
more than one process to update the cells on each generation. You will
probably want to allocate some region of the grid to each process.

There are two essentially different ways to structure the program:

   1. as a shared-variable synchronous data parallel program

   2. as a pure message-passing program with cells 
   represented by individual processes

In both cases the processes need to be synchronised in some way,
so that the rules of the game are followed. You need to think
carefully about how to avoid race conditions. 

Test your implementation by considering some interesting seeds (see the
Wikipedia page to get some ideas).

#### Optional

Implement one or more of the variants from the Wikipedia page: you
might have to adapt `Display` to do so. Alternatively, devise a
more complicated game that can be modelled using cellular automata. For
example, you could model the interactions between foxes and rabbits.
Don't make the rules too complicated, though! You could include some
randomness in the game.

#### Solutions
Two solutions appear here:

1. The implementation **Life**  is a shared-variable 
synchronous data-parallel program using barrier synchronization.

2. The implementation **Lyfe** is a pure 
message-passing program, with each cell represented by an
individual process. 

Usage of the programs (from SBT) is:

`runMain Life -n«count»`     Uses «count» columns and rows in a square, 
and starts off with a random collection of live cells.  Run the
program with `-h` to see details of other paramater settings.
    
`runMain Lyfe «cols»X«rows» «width»x«height»` Uses «cols» columns 
and «rows» rows in a rectangle of dimension «width»x«height» pixels.
If there is room for more columns or rows in the given rectangle, then 
one of (cols, rows) is increased so as to fill the rectangle. Default
parameters are 80X80 800x800. Living cells are coloured by age: newborn cells 
are green; cells that have lasted up to 7 generations are red; cells that have 
lasted beyond 7 generations are coloured blue.

The simulation can be stopped and started, and
cells can be "painted" alive when it is stopped. For details run
the program with `-h.` 

#### Performance

**Life:**
The default parameters (65536 cells in a square array) give (astonishingly) 
good performance. If you  want to see what is happening in detail it's a good 
idea to increase the  frame dwell time from `-f10` to `-f100` (units are 
in milliseconds).

**Lyfe:** 
The defaults (80X80) give very good performance; but be aware that
the fundamental limitation of this approach is that the display is
a generic component that "polls" each of the `N` cells for its image
once per generation, and that the number of intercell messages 
per generation is  `NxN`. Within each cell the messages are sent 
in parallel and  received sequentially. At 100X100 performance is
acceptable. At 256X256 (65K cells) performance is just about 
adequate.

**Note:** performance descriptors are from runs made on a 32G (3.2GHz Intel)
Mac Mini.





#### Background
An early version of the **Lyfe** implementation
was hampered by the (mandatory) use of kernel threads, and could
manage only a relatively small grid. Before the
advent of *virtual threads* we had written:
"Experience suggests suggests that when using kernel threads,
allocating a process to each square in the grid means that only a
small grid can be dealt with before the operating system or the jvm
begins to run out of thread resources or gets swamped by 
context-switching overhead."
and that: 
"Using virtual threads would be a better bet for
a serious attempt (and we will present one due to Jones and Goldsmith
(Programming in **occam2**) in due course)."

Other message-passing solutions might have workers working on larger rectangular regions
of the grid, and exchanging the edges of their regions with neighbour
workers on every generation.

## Additional Examples

### Boids

This is an example of a hybrid concurrent implementation using 
[ThreadCSO](https://github.com/sufrin/ThreadCSO)
barriers, semaphores, and channels
to simulates a collection of flocking birds, as
originally specified in [Boids](https://en.wikipedia.org/wiki/Boids).

Once started it displays the flock at the specified framerate, and permits
various parameters to be set dynamically from the keyboard of the terminal
from which it was started.  Each bird is simulated by a single
threadcso process, as are the display controller, the interaction
controller, and the keyboard.

Birds and the display synchronise on a barrier (twice per displayed frame), and the
keyboard communicates by a channel with the interaction controller that can
set the flock characteristics.  There is no synchronisation involved here,
though a purist might complain that different birds might thereby read
different parameters during the same display cycle.

Usage (from sbt) is:

`runMain Boids` [*args*]

Each *arg* specifies wither the flock size (as a number), the window dimensions (*width*`x`*height*),
or one of the flock characteristics.

If no *args* are given then the flock characteristics and window size are taken to be

   250 1000x700 R=40 S+ A+ drag=0.900 co=0.00200 se=8.00 al=0.0500 f=0.750 r=0150 V=10.0 v=5.00

Flock characteristics are as follows:

          drag=0.9      drag coefficient
          co=0.001      Cohesiveness
          se=8.0        Min separation
          al=0.05       Tendency to align with visible neighbours
          f=0.7500      Field of vision (as a fraction of π)
          r=150.0       Range of vision
          V=10.0        Max velocity
          v=5.0         Min velocity  

They are changed from the keyboard by typing (individually or together) commands of the form

*characteristic*`=`*value*

For example:

          f=0.25 r=50.0

reduces the field and range of vision of the birds.

Additional parameters pertain to the details of the simulation. Apart from `R` they
appear to have little effect.

          S+            synchronising (+) or not (-)
          A+            antialiasing (+) or not (-)
          R=40          frame rate (ms/frame)

### Gravitation

A demonstration of pseudo-gravitational particle calculations done
by several workers in lock-step, coordinated by barriers. A
description of the technique used for particle computations is given
in the Particle Computations section of the lecture notes on
[Synchronous Data Parallel Programming](https://github.com/sufrin/ThreadCSO/blob/main/Lectures/03-dataparallel.pdf).

  
  * The number of particles is the product of the number of
  worker processes and twice the "quota" of particles for each
  worker.
  
  * The particles move in a closed container with energy absorbent
  walls.

  * Denser particles are darker.

  * The simulation can be stopped or started from the GUI
  by pressing the space bar or clicking on the "Running" checkbox.

  *  When the simulation is stopped, individual particles can be selected
  by mouse clicking on them. A mouse click away from any particle clears
  the selection; otherwise each mouse click adds to the selection. Clicking
  on a particle with the Ctrl/Cmd shift pressed deselects the particle
  if it was selected, and selects it if it wasn't selected. Ctrl-A (and
  Cmd-A) selects all particles. 

  The density, radius or velocity of selected particles can be changed by
  rotating the mousewheel in the appropriate direction, with the
  appropriate radiobutton (Radius/Density/Speed) selected. 


Several parameters are dynamically settable from the GUI.  

  1. `C` is the upper bound on the speed to which particles can accelerate

  1. `Wall` is the coefficient of restitution of walls.

  1. `Bounce` is the "bounciness" of particles. Initially set to a
  factor that almost reverses the force on particles that are
  touching/overlapping.
  
  1. δt is the simulated elapsed time between computed frames.

  1. FPS is the (target) number of frames to be shown per elapsed
  second of real time. This is really here to test the efficiency
  of our scala code, and under some circumstances it may not be
  reached; but this doesn't damage the simulation. Feedback is given
  in the "Overrun" field of the user interface: the number given
  is the average  of the last 100 overrun times (in μs) since FPS
  changed.

  
Usage (from sbt) is

`runMain Particles` [*args*], where each *arg* is one of

        q num -- each worker manages 2xnum particles
        w num -- number of workers is num
   
The initial position and radii of most particles is chosen randomly.
Initial radii are intended to give a decent range that depends
on the numbers of particles and the width of the display.


Example:

  * 4 workers 8 particles

        runMain Gravitation                          

This is the one to play with first. Try making one of the particles
very dense; then try setting `Repel`, and/or setting the wall's
momentum multiplier to a fraction > 1.

Other examples:

  * 2 particles managed by a single worker

        runMain Gravitation w 1 q 1

  * 512 particles 
  
        runMain Gravitation w 32 q 8

Things can get a bit hectic when gravitation is high and there are lots of
particles; but you can moderate behaviour from the GUI.

When the `MX` (mass exchange) feature is set and a heavy particle
and a lighter particle intersect for 5 ticks or more, about 20% of
the heavier particle's mass is transferred to the lighter particle.

### Autonomous

A like `Gravitation`, save that each body is
simulated by its own process that (by default) computes its next
visible state when it receives `Tick` messages from the controller.
There are (presently) two distinct kinds of body, namely sphere and immobile;
but the programme framework is set up to support more than just
these kinds.

The `Gravitation` interface is supplemented as follows:


  1. Selected particles are deleted in response to `Delete` or `Backspace`.

  2. Immobile particles are added at the cursor in response to `z`, and mobile
  particles are added in response to `a`.


Example:

  * No preset immobiles or spheres

        runMain Autonomous


  * 20 immobiles, 128 spheres

        runMain Autonomous i 20 s 128

The initial position of each body is chosen randomly.
Initial radii are intended to give a decent range that depends
on the numbers of particles and the width of the display.