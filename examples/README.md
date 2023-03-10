# Some CSO examples

*The code provided here is compatible with Scala 2.13.  It will
compile with the most recently-published variant of the threadcso
library jar, and will run without change using **jdk 20**. Compiled
code will also run -- using Kernel threads rather than Virtual threads
by default -- using earlier versions of jdk.*

*Explanations of some of the principles behind the examples, and
of **threadcso** itself, can be found in the accompanying [lecture
notes](https://github.com/sufrin/ThreadCSO/Lectures).*


Bernard Sufrin, Oxford. 2011, 2017, 2023

## Solutions to Practicals

In this section we have collected solutions to practical problems
set as part of the Concurrent & Distributed Programming course at
the University of Oxford that was delivered either by Bernard Sufrin
or by Gavin Lowe between the years 2004 and 2022. 

The course content variedPracticals were accompanied
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
automaton invented by John Conway in 1970. The Wikipedia entry on Life

        http://en.wikipedia.org/wiki/Conway's_Game_of_Life

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

We will consider a variant with a finite $N$ by $N$ grid, treated as a
toroid, i.e. where the top and bottom edges of the grid are treated as
being adjacent, as are the left and right edges.

#### Your task 

Your task is to implement the Game of Life. Your program should use
$p$ processes to update the cells on each generation. You will
probably want to allocate some region of the grid to each process.

There are two essentially different ways to structure the program:

   1. as a shared-variable synchronous data parallel program

   2. as a pure message-passing program with cells represented by individual processes

In both cases the processes need to be synchronised in some way,
so that the rules of the game are followed, so you need to think
carefully about how to avoid race conditions. 

There is a file `Display.scala` (on the course website), which defines a
`Display` class that can be used to build displays that show the state
of a grid. A display is created by

        val display = new Display(N, a)

where is an integer, representing the size of the grid, and is the grid,
represented by an by array of , e.g. initialised by

        val a = Array.ofDim[Boolean](N,N)

The display is made consistent with by executing

        display.draw

This shows solid black squares at positions where $a$ is true, and grey
squares where it is false.

Test your implementation by considering some interesting seeds (see the
Wikipedia page to get some ideas).

#### Optional

Implement one or more of the variants from the Wikipedia page: you
might have to adapt `Display` to do so. Alternatively, devise a
more complicated game that can be modelled using cellular automata. For
example, you could model the interactions between foxes and rabbits.
Don't make the rules too complicated, though! You could include some
randomness in the game.

#### Advice (2023)

Experience suggests suggests that when using kernel threads,
allocating a process to each square in the grid means that only a
small grid can be dealt with before the operating system or the jvm
begins to run out of thread resources. Using virtual threads (as
provided by recent versions of threadcso) would be a better bet for
a serious attempt (and we will present one due to Jones and Goldsmith
(Programming in **occam2**) in due course). Other message-passing
solutions will have workers working on larger rectangular regions
of the grid, and exchanging the edges of their regions with neighbour
workers on every generation.


## Additional Examples

### Particles


A demonstration of pseudo-gravitational particle calculations done
by several workers in lock-step, coordinated by barriers, with
(many) parameters of the simulation settable from the GUI.  A
description of the technique used for particle computations is given
in the Particle Computations section of the lecture notes on
[Synchronous Data Parallel Programming](https://github.com/sufrin/ThreadCSO/blob/main/Lectures/03-dataparallel.pdf).

  
  1. The particles move in a closed container with energy absorbent
  walls. 

  1. There is an upper bound on the speed to which they can accelerate

  1. Clicking on a particle increases its density by an order of
  magnitude (and changes its hue "redward". The mass of a particle
  is calculated in the usual way from its radius, *viz*:
  density×(4/3)πR&#x00B3;
  
  1. Control-clicking on a particle  decreases its density by an order of magnitude.
  
  1. One or more particles can be selected (or deselected) by Shift-clicking.

  1. When the simulation is not running, selected particles can be
     nudged, by using the direction keys on the keyboard. Their radius
     can be decreased by pressing `1`, or  increased by pressing `2`.
     Radii of *all* particles is decreased (increased) by pressing
     control-`1` (control-`2`).


  1. Coefficients of restitution of walls and particles can be set
     interactively or as the application begins.

  1. δt is the simulated elapsed time between computed frames

  1. FPS is the (target) number of frames to be shown per elapsed
  second of real time This is really here to test the efficiency
  of our scala code, and under some circumstances it may not be
  reached; but this doesn't damage the simulation.

  1. The number of particles is the product of P (the number of
  worker processes), and 2×S (the number of particles managed per
  worker process). 
 
Usage (from sbt) is

`package examples; runMain Particles` [*args*], where each *arg* is one of

   S «S: int» (default 1) each worker manages 2xS particles

   P «P: int» (default 4) number of worker processes

   t «∂t: double» (default 3.0) time quantum for simulation

   s «scale: int» (default -11) order of magnitude of 'gravitational' constant: G = 6.79E<scale>

   W «Wall: double» (default 0.9) When it hits a wall a particle's momentum *= -Wall.

   B «Ball: double» (default 1.0) force multiplier for touching particles [negative => repulsion]

   FPS «FPS: int» (600) frames to be shown per second (target value)

   w=«int» (800) width of the arena (units)

   h=«int» (700) height of the arena (units)

   C «CF: double» (16.0) particle speed is limited to `|`(width/CF∂t, height/CF∂t)`|`.

   -d enable the debugger (false)

   `--` the remaining parameters are taken to be particle specifications of the form `<radius>@<x>,<y>`.

A total of 2×P×S particles participates in the simulation. The
initial positions of the non-specified particles are chosen randomly.
Their initial radii are intended to give a decent range that depends
on the numbers of particles and the width of the display.


Default example

        runMain Particles                          # 4 workers 8 particles

This is the one to play with first. Try making one of the particles very dense; then try setting
`Repel`, and/or setting the wall's momentum multiplier to a fraction > 1.

Other examples:

  * Two very large particles managed by a single worker

        runMain Particles P 1 -- 250@150,150 40@200,200

  * Larger numbers of particles
  
        runMain Particles s -9 P 40                # 80 particles, G = 6.79E-9
        runMain Particles S 4  P 40  w=1800 h=1000 # 320 particles, G = 6.79E-11 

Things can get a bit hectic when gravitation is high and there are lots of
particles; but you can moderate behaviour from the control panel of the
viewer. When there are too many particles on a small screen they can be
indistinguishable at first -- the screen just looks like a greenish blob.
THis is remedied by 

