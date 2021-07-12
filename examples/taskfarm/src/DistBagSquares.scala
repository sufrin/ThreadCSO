object DistBagSquares
{ import DBOT._; import io.threadcso._  
  def main(args: Array[String])
  { var N = 3
    var S = 4
    for (arg <- args)
      if (arg.matches("[0-9]+")) N = arg.toInt
      else
      if (arg.matches("-[0-9]+")) S=arg.substring(1).toInt

    val solns = N2N[MagicSquare](1, 1, "Solutions")
    
    val link = // links between nodes (indexed by destination)
        for (i<-0 until N) yield OneOne[Message[MagicSquare]](s"${(N+i-1)%N}->$i") 
    
    val ring = // The ring of nodes
        (  Node(0, link(0), link(1), solns, new MagicSquare(S))
        || ||(for (i <- 1 until N) yield Node(i, link(i), link((i+1)%N), solns)))
                      
    val report = proc ("report") // print the finished tasks -- the solutions
    { repeat { val b = solns?(); b.print } }
                  
    println(debugger)
    (ring || report)()
    exit
  }
}

