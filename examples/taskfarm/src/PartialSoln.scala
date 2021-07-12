/**
        Partial Solution to the Magic Squares problem
*/
class PartialSoln(size: Int) {
  // board is an size*size matrix, with each entry board(i)(j) containing either
  // (a) a number in the range [1..size] to indicate that that number has been
  // played in (i,j), or (b) 0, to indicate that square (i,j) is empty.  
  private var board: Array[Array[Int]] = null;

  // free is an array of size max+1, where entry k is true if the number k has
  // not yet been placed on the board.  
  // Invariant: free(k) <=> (exists i, j: 0 <= i,j < size: board(i)(j) = k)
  private var free: Array[Boolean] = null;

  private val max = size*size         // maximum entry
  private val sum = size*(max+1)/2 // sum of rows/columns/diagonals

  // minSum[k] is sum of k smallest values i s.t. free[i]
  // maxSum[k] is sum of k largest values i s.t. free[i]
  private var minSum: Array[Int] = null;
  private var maxSum: Array[Int] = null;

  // Initialisation with an empty board
  def empty: PartialSoln = {
    board = Array.ofDim[Int](size,size)
    for (i <- 0 until size; j <- 0 until size) board(i)(j) = 0;
    free = Array.ofDim[Boolean](max+1)
    for (k <- 1 to max) free(k) = true
    minMaxSums
    this
  }

  // Calculate minSum and maxSum
  private def minMaxSums = {
    minSum = new Array[Int](size+1); maxSum = new Array[Int](size+1);
    var k = 1; // Invariant: have calculated minSum[1..k), maxSum[1..k)
    var i = 1; // Invariant: there are k free values in [1..i)
    var j = max; // Invariant: there are k free values in [j+1..max+1)
    var theMinSum = 0; // Sum of smallest k free values 
    var theMaxSum = 0; // Sum of largest k free values 

    while (k<size) {
      while (i<max+1 && !free(i)) i+=1
      if (i<max+1) { theMinSum+=i; minSum(k)=theMinSum; i+=1; }
      while (j>=0 && !free(j)) j-=1
      if (j>=0) { theMaxSum+=j; maxSum(k)=theMaxSum; j-=1 }
      k+=1
    }
  }
      
  // Is the board finished
  def finished: Boolean = {
    for (k <- 1 to max) if (free(k)) return false;
    return true
  }

  // Is it legal to play piece k in position (i,j)
  def isLegal(i: Int, j: Int, k: Int) : Boolean = {
    if (!free(k) || board(i)(j)>0) return false
    // check this row
    var s = k; // sum of row
    var empties = 0; // # empty spaces in this row, including (i,j)
    for (j1 <- 0 until size) {
      s += board(i)(j1); if (board(i)(j1)==0) empties+=1
    }
    if (empties==1 && s!=sum || 
       empties>1 && (s+minSum(empties-1) > sum || s+maxSum(empties-1)<sum)) 
      return false;
    // check this column
    s = k; empties = 0;
    for (i1 <- 0 until size) {
      s += board(i1)(j); if (board(i1)(j)==0) empties+=1
    }
    if (empties==1 && s!=sum || 
       empties>1 && (s+minSum(empties-1) > sum || s+maxSum(empties-1)<sum))
      return false;
    // check leading diagonal
    if (i==j) {
      s = k; empties = 0;
      for (i1 <- 0 until size) {
        s += board(i1)(i1); if (board(i1)(i1)==0) empties+=1
      }
      if (empties==1 && s!=sum || 
         empties>1 && (s+minSum(empties-1) > sum || s+maxSum(empties-1)<sum))
        return false;
    }
    // check other diagonal
    if (i+j==size-1) {
      s = k; empties = 0;
      for (i1 <- 0 until size) {
        s += board(i1)(size-1-i1); if (board(i1)(size-1-i1)==0) empties+=1
      }
      if (empties==1 && s!=sum || 
         empties>1 && (s+minSum(empties-1) > sum || s+maxSum(empties-1)<sum))
        return false;
    }
    return true
  }

   // Return new partial solution obtained by placing k in position (i,j)
  def doMove(i: Int, j: Int, k: Int) : PartialSoln = {
    // pre: isLegal(i,j,k)
    val newBoard = Array.ofDim[Int](size,size);
    for (i1 <- 0 until size; j1 <- 0 until size) newBoard(i1)(j1) = board(i1)(j1);
    newBoard(i)(j) = k;
    val newFree = new Array[Boolean](max+1);
    for (k1 <- 1 to max) newFree(k1) = free(k1);
    newFree(k) = false;
    val ps = new PartialSoln(size);
    ps.board = newBoard; ps.free = newFree;
    ps.next_i = this.next_i; ps.next_j = this.next_j;
    ps.minMaxSums;
    return ps;
  }

  // ---------
  var next_i = 0; var next_j = 0; // next position to play
  // Choose next position to play; just iterate through squares
  def choose: (Int,Int) = {
    val res = (next_i, next_j);
    next_j += 1;
    if (next_j==size) { next_j = 0 ; next_i += 1; };
    res
  }

  // ---------
  // Print this solution
  def printSolution: Unit = {
    println
    for (i <- 0 until size) {
      for(j <- 0 until size) print("%02d  ".format(board(i)(j)));
      println
    }
  }

  override def toString: String =
  {
    val b = new StringBuilder()
    for (i <- 0 until size)
    {
      b.append(board(i).mkString(" "))
      b.append("\n")
    }
    b.toString
  }
}
