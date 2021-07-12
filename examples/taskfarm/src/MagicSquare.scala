  class MagicSquare(size: Int, wrap: PartialSoln = null) 
  extends DBOT.Task[MagicSquare]
  {  val soln = if (wrap==null) new PartialSoln(size).empty else wrap
     def finished: Boolean = soln.finished
     def subtasks  =
     { val (i,j) = soln.choose
       for (k <- 1 to size*size if (soln.isLegal(i,j,k))) yield 
           new MagicSquare(size, soln.doMove(i,j,k)) 
     }
     override def toString = soln.toString
     def print = soln.printSolution
  }
