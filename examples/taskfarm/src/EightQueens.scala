object EightQueens
{ import DBOT._
  import io.threadcso._
  
  class Partial(N: Int) extends Task[Partial] {
    // Represent a partial solution (a task) by a list of Ints whose
    // i'th entry represents the row number of the queen in column i 
    private var board: List[Int] = Nil
    private var len = 0
  
    def finished: Boolean = (len==N)
  
    private def isLegal(j: Int) = {
      // is piece (i1,j1) on different diagonal from (len,j)?
      def otherDiag(p:(Int, Int)): Boolean= { 
        val (i1, j1) = p
        i1-len!=j1-j && i1-len!=j-j1
      }
      
      (board forall ((j1: Int) => j1 != j)) &&       // row j not already used
      (List.range(0,len) zip board forall otherDiag) // diagonals not used
    }
  
    // New partial solution resulting from playing in row j
    private def doMove(j: Int): Partial = {
      val newPartial = new Partial(N)
      newPartial.board = this.board ::: (j :: Nil)
      newPartial.len = this.len+1
      newPartial
    }
    
    
  
    // Every subtask is an legal extension of this partial solution
    def subtasks = 
    {   for (j <- 0 until N if (isLegal(j))) yield doMove(j)
    }
  
    override def toString: String = {
      var st = "/";
      for (i <- 0 until len) st = st + (i, board(i))+"/";
      return st
    }
  }
  
  def main(args: Array[String])
  { var N = 3
    var S = true
    for (arg <- args)
      if (arg.matches("[0-9]+")) N = arg.toInt
      else
      if (arg.matches("B")) S=false
    val solns = N2N[Partial](1, 1, "Solutions")
    
    // Channels that form the ring
    val link: Seq[Chan[Message[Partial]]] =
        for (i<-0 until N) yield
            if (S) OneOne[Message[Partial]](s"${(N+i-1)%N}->$i")
            else OneOneBuf[Message[Partial]](1, s"${(N+i-1)%N}->$i") // indexed by recipient's id
    
    // The ring of Nodes
    val ring = 
        (  Node(0, link(0), link(1), solns, new Partial(8)) ||
        || ( for (i <- 1 until N) yield 
                  Node(i, link(i), link((i+1)%N), solns) ))
                                    
    println(debugger)
    (ring || component.console(solns))()
    exit
  }
}
