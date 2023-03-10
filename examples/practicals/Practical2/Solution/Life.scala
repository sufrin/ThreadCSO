import io.threadcso._

/**
        Model solution for practical 2. 
        
        Some acceptable variations are noted in my comments. 
        Other variations may well be acceptable, and I leave this
        judgment to the demonstrators.
        BS (January 2016)
        
*/
object Life
{
  val N          = 256                       // size of the grid
  
  val grid       = Array.ofDim[Boolean](N,N)
  


  /** Successor on toroidal grid */
  @inline private def succ(k: Int) = (k+1)%N
  
  /** Predecessor on toroidal grid */
  @inline private def pred(k: Int) = (k+N-1)%N
  
  /** 1 if cell(i, j) is alive, else 0 */
  @inline private def ##(i: Int, j: Int) = if (grid(i)(j)) 1 else 0

  // parameters: can be set on the command line at startup
  var p          = 64   // number of processes (must divide N)
  var frameDwell = 10L  // how long to wait before next frame
  //
  var height = N/p      // height of one strip
  
  // Barrier: constructed at startup
  var barrier: Barrier = null
  
  /**
       Worker for the global grid region rows [me*height..(me+1)*height]

       On each round
       1: calculates the next generation of its region into a local array
       2: copies the local array back to its region
       3: (me==0 only) draws the grid and pauses for a "dwell time" so it can be seen
          this "dwell" in worker 0 is overlapped with phase 1 in the other workers
  */
  
  def Worker(me: Int, display: Display) = proc (s"Worker($me)") {
    val start = me*height
    val end   = (me+1)*height
   
    //  Worker's private copy of the next generation of its region of the grid
    val nextGen = Array.ofDim[Boolean](height, N)
    
    /** NB: my rationale for allocating a (constant) nextGen per
        worker OUTSIDE its main loop is based on my judgment that it is 
        probably faster to copy its new values back to
        the (global) grid on each iteration than it would 
        be to reallocate nextGen on each iteration.
    */

    while (true) {

      //** Calculate next generation rows            
      for (i <- start until end; j <- 0 until N) {
        val neighbours = // number of live neighbours
          ##(pred(i), pred(j)) + ##(pred(i), j) + ##(pred(i), succ(j)) +
          ##(i,       pred(j)) +                  ##(i,       succ(j)) +
          ##(succ(i), pred(j)) + ##(succ(i), j) + ##(succ(i), succ(j))
        nextGen(i-start)(j) = (grid(i)(j) && neighbours==2) || neighbours==3
      }
      
      barrier.sync()
      for (i <- start until end) 
      {   val nextRow = nextGen(i-start)
          
          // copy new row values back to the global grid
          for (j <- 0 until N) grid(i)(j) = nextRow(j)
          
          // NB: The whole-row assignment grid(i) = nextRow can be done providing
          // that the nextGen declaration is moved to //** above
             
      }
      barrier.sync()
      if (me==0) { 
         display.draw
         sleep(frameDwell*milliSec) 
      }
    }
  }


  // Various constructions of initial configurations
  
  // Produce initial configuration from list of strings, where each string
  // corresponds to one row, and each "*" corresponds to a live cell
  def ParseStrings(ss: List[String])(x: Int, y: Int): Unit = {
    var y1=y
    for (s <- ss){
      var x1 = x
      for (c <- s){ if (c=='*') grid(x1)(y1) = true; x1 = succ(x1) }
      y1 = succ(y1)
    }
  }

  // Make glider centred at (pred(x),y); the "pred" is for
  // backwards-compatability
  def MakeGlider(x: Int, y: Int) = 
      ParseStrings("***"::"*.."::".*."::Nil)(pred(x), y)

  // Make block at (x,y)
  def MakeBlock = ParseStrings("**"::"**"::Nil) _

  // Initialise randomly
  def makeRandom(): Unit = {
    val random = new scala.util.Random
    for(y <- 0 until N*N/5) grid(random.nextInt(N))(random.nextInt(N)) = true
  }

  // Create the following pattern, centred at (x,y)
  // XXX.X
  // X....
  // ...XX
  // .XX.X
  // X.X.X
  
  // This is supposed to lay a sequence of blocks, but doesn't seem to work
  def BlockLayer1 = 
      ParseStrings("***.*"::
                   "*...."::
                   "...**"::
                   ".**.*"::
                   "*.*.*"::Nil
                  ) _

  // Gosper gun, which fires off a sequence of gliders
  def GosperGun = 
    ParseStrings(
      "........................*..........."::
      "......................*.*..........."::
      "............**......**............**"::
      "...........*...*....**............**"::
      "**........*.....*...**.............."::
      "**........*...*.**....*.*..........."::
      "..........*.....*.......*..........."::
      "...........*...*...................."::
      "............**......................"::Nil
    ) _
 
  def main(args:Array[String]) = {
    // parse argument to choose option
    println("Life: -h for help")
    if (args.length>0) {
      for (arg <- args) 
      if (arg=="a"){ MakeGlider(35,30); MakeBlock(27,27) } 
      // quickly reaches fixed point 
      else if (arg=="b"){ MakeGlider(34,30); MakeBlock(27,27) }
      // after long history, reaches oscillating state
      else if (arg=="c"){ MakeGlider(33,30); MakeBlock(27,27) }
      // quickly dies out
      else if (arg=="d"){ MakeGlider(32,30); MakeBlock(25,25) }
      // quickly dies out
      else if (arg=="e"){ MakeGlider(31,30); MakeBlock(25,25) }
      // quickly dies out
      else if (arg=="f"){ MakeGlider(30,30); MakeBlock(25,25) }
      // quickly dies out
      else if (arg=="g"){ MakeGlider(29,30); MakeBlock(25,25) }
      // quickly dies out
      else if (arg=="h"){ MakeGlider(28,30); MakeBlock(25,25) }
      // quickly dies out
      else if (arg=="i"){ MakeGlider(27,30); MakeBlock(25,25) }
      // after long history, reaches oscillating state; same as 2?
      else if (arg=="j"){ MakeGlider(26,30); MakeBlock(25,25) }
      // quickly reaches fixed point; same as 1?
      else if (arg=="k"){ MakeGlider(25,30); MakeBlock(25,25) }
      // quickly reaches fixed point
      else if (arg=="l"){ MakeGlider(24,30); MakeBlock(25,25) }
      // glider misses target
      else if (arg=="bl"){ BlockLayer1(25,25) }
      // block laying machine; this doesn't seem to work
      else if (arg=="gg"){ GosperGun(25,25) }
      // fires various gliders ...
      else if (arg=="r") { makeRandom() }
      else if (arg.matches("-w[0-9]+")) p = arg.substring(2).toInt
      else if (arg.matches("-f[0-9]+")) frameDwell = arg.substring(2).toLong
      else if (arg.matches("-.*"))
           { println(s"life -w<nworkers:$p> | -f<framedwellms:$frameDwell> | [a-l] | bl | gg | r")
             exit()
           }
      else makeRandom()
    }
    else makeRandom()
    
    height = N/p
    if (N % p == 0)
    { val display = new Display(N, grid)          // set up display
      barrier = new Barrier(p)
      display.draw
      run(|| (for (i <- 0 until p) yield Worker(i, display)))
    } 
    else
    { println(s"Workers ($p) should divide the size of the world ($N) exactly")
      exit(1)
    }

  }
}











