

/**
 * Prototype of a `Boid`. All functioning `Boid`s share its characteristics.
 */
object Boid extends BoidParams {
  type State = (Double, Double, Double, Double) // x, y, speed, theta
}

class BoidParams {
  import scala.math.Pi
  
  // Frame delay
  var Rate = 40
  
  // parameters affecting degree of reaction to other Boids, corresponding
  // respectively to cohesion, separation, and alignment
  var Coh = 0.002
  var Sep = 8.0
  var Align = 0.05
  
  // max and min speed of a boid
  var maxSpeed = 10.0
  var minSpeed = 5.0
  
  // breadth and range of the boid's field of vision
  var breadth = 3*Pi/4
  var range   = 150.0
  
  // Factor by which speed is multiplied at each step, because of "drag"
  var drag = 0.9
}

class Boid {
  import Boid._ 
  import Math.{min, max, sin, cos, abs, sqrt, atan2, pow}  
  import scala.math.Pi
  import BBox._
  
  // State of the boid
  var x, y:       Double = 0.0 // current position
  var speed:      Double = 0.0 // current speed
  var direction:  Double = 0.0 // current heading 
                               // (radians clockwise from vector (0,1), per atan2 convention)

  // Initialise the state randomly (using the given Random generator)
  def init(random: scala.util.Random) = {
    x = xSize * random.nextDouble()
    y = ySize * random.nextDouble()
    speed = minSpeed + (maxSpeed-minSpeed) * random.nextDouble() 
    direction = 2 * Pi * (random.nextDouble() - 0.5)
  }

  // Set the state of this boid to the given state
  def setState(state: Boid.State): Unit = state match
  { case (x: Double, y: Double, speed: Double, direction: Double) => 
         setState(x, y, speed, direction)
  }
  
  // Set the state of this boid to the given parameters
  def setState(x: Double, y: Double, speed: Double, direction: Double): Unit = {
    this.x = x; this.y = y; this.speed = speed; this.direction = direction
  }

  // Get the state of this boid
  def getState:  Boid.State = (x, y, speed, direction)

  // mod operation treating negative values correctly
  // assumes -modV <= x 
  @inline private def mod(x:  Double, modV:  Int):  Double = (x+modV) % modV

  @inline private def square(x1:  Double):  Double = x1*x1

  @inline private def mag(x:  Double, y:  Double):  Double = sqrt(square(x)+square(y))

  // Find min in absolute terms of x1, x1+size, x1-size
  // assumes size >= 0
  @inline private def norm(x1:  Double, size:  Double):  Double = 
    if (abs(x1)>abs(x1+size)) x1+size else 
    if (abs(x1)>abs(x1-size)) x1-size else x1

  // Difference between x1 and x (resp. y1 and y) on torus
  @inline private def deltaX(x1:  Double) = norm(x1-x, xSize)
  @inline private def deltaY(y1:  Double) = norm(y1-y, ySize)
    
  // Is point (x1,y1) within the bird's field of view?
  private def isInView(x1:  Double, y1:  Double):  Boolean = {
    // position relative to the boid in torus topology
    val dx = deltaX(x1); val dy = deltaY(y1)
    
    if (square(dx) + square(dy) > square(range)) return false; // too far away
    
    // Rotate coordinates so that boid's mid line is in direction 0
    val direction1 = atan2(dx,dy); // direction of (x,y) from (x0,y0)
    val offSet0    = abs(direction-direction1);
    val offSet     = min(offSet0, 2*Pi-offSet0); // angle between direction and direction1
    return offSet<=breadth
  }

  // Calculate new state for this boid from the current states of all Boids.
  def newState(allBoids:  Iterable[Boid]): Boid.State = {
    var forceX     = 0.0; var forceY     = 0.0 // cohesion and separation force components
    var alignmentX = 0.0; var alignmentY = 0.0
    
    // Sum of velocities of neighbouring Boids, used for alignment
    var count = 0; // number of visible Boids
    for (b <- allBoids; if b!=this) {
      if (isInView(b.x, b.y)) {
        val coh = Coh
        val sep = Sep
        val dx = deltaX(b.x); val dy = deltaY(b.y)
        val distSquared = max(dx*dx+dy*dy, 0.00001)
        forceX += (coh-sep/distSquared)*dx; forceY += (coh-sep/distSquared)*dy
        alignmentX += sin(b.direction); alignmentY += cos(b.direction)
        count += 1
      }
    }
    
    // New velocity based on forceX and forceY
    val velx       = drag * speed * sin(direction) + forceX
    val vely       = drag * speed * cos(direction) + forceY
    val newSpeed   = max(min(mag(velx, vely), maxSpeed), minSpeed)
    val direction0 = atan2(velx,vely)
    
    // Now consider alignment effect
    val avTheta = atan2(alignmentX, alignmentY)
    val Alignp = Align*mag(alignmentX, alignmentY) / pow(count, 0.9)
    //   min(count,10)/10; // weighting for alignment 
    val newTheta = if (count>0) Alignp*avTheta + (1.0-Alignp)*direction0 else direction0
    val newX = mod(x + speed*sin(direction) , xSize)
    val newY = mod(y + speed*cos(direction) , ySize)
    return (newX,newY,newSpeed,newTheta)
  }
}









