# Boids
## Bird flock simulation using ThreadCSO

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
set the bird parameters.  There is no synchronisation involved here,
though a purist might complain that different birds might thereby read
different parameters during the same display cycle.  

### Running

The main program is known as `Boids`, and can be started in any of the usual
ways, including:

  1. from the sbt command line with `examples / runMain Boids`*parameterss*
  2. from the shell command line with `scala -cp` *classpath* `BOIDS` *parameters*

Where *classpath* includes the path to the `threadcso` library jar, and the
path to the `examples` jar.


### Parameters
The *parameters* can include any of those described below, as well as

  * *width*x*height* to set the size of the window 
  * *number* to set the number of birds (default is 200)
  
One or more parameters can be set on a single keyboard line at any
time: just write a sequence of paramname=value settings separated
by spaces. The initial default parameter values are:

          S+            synchronising (+) or not (-)
          A+            antialiasing (+) or not (-)
          R=40          frame rate (ms/frame)
          drag=0.9      drag coefficient
          co=0.002      Cohesiveness
          se=8.0        Min separation
          al=0.05       Tendency to align with visible neighbours
          f=0.7500*π    Field of vision
          r=150.0       Range of vision
          V=10.0        Max velocity
          v=5.0         Min velocity
          
Watch what happens as you (gradually) change parameters. All sorts of
interesting emergent behaviours can be observed.

Try flocks with much larger numbers than the default. When numbers are
above about 2000 the limitations of the current (linear) implemntation
of `Barrier` may start to show.

*Bernard Sufrin, February 2016 and March 2023*

**HISTORY**: Gavin Lowe used a simpler variant of Boids as an example
of Barrier synchronization sometime during his first tenure of the
Oxford Concurrent Programming course. Gavin's variant had race
conditions around the use of the display that led to bizarre visual
artefacts due to severe problems of synchronization between the
Java GUI thread and the program itself. 

