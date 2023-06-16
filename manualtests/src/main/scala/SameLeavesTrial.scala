import io.threadcso._

/**
  * A little program showing how streams read from channels may be used to compare traversals of
  * trees without having to reify entire traversals.
  */

object SameLeavesTrial {

  /** Compare the streams `inl` and `inr`, stopping as soon as they differ, and
    * writing whether they differ to `ans`.
    */
  def sameStreams[T](inl: ??[T], inr: ??[T], ans: !![Boolean]) = proc {
    var l,  r  = inl.nothing // last-read value
    var ln, rn = 0           // number of values read

    val nextPair: PROC =
      proc { l = inl ? (); ln = ln + 1 } ||
      proc { r = inr ? (); rn = rn + 1 }

    var same = true
    repeat(same) { nextPair(); same = l == r }
    
    ans ! (same && ln == rn)

    ans.closeOut();
    inl.closeIn();
    inr.closeIn()
  }

  /** An abstraction of a not-necessarily reified tree */
  trait Tree[V] {
    def subtrees(): Seq[Tree[V]]
    def value: V
    def isLeaf: Boolean

    private def depthFirstTo(out: !![V]): Unit =
      if (isLeaf) out ! value else for (t <- subtrees()) t.depthFirstTo(out)

    def depthFirst(out: !![V]): Unit = {
      attempt { this.depthFirstTo(out) } { () }
      out.closeOut()
    }
  }

  /** Test whether two trees have the same depth-first traversal */
  def sameLeaves[V](tl: Tree[V], tr: Tree[V]): Boolean = {
    val left, right = OneOne[V]
    val answer      = OneOneBuf[Boolean](1)

    // Set up and start the network
    (    proc { tl.depthFirst(left) }
      || proc { tr.depthFirst(right) }
      || sameStreams(left, right, answer)
    ) ()

    // Collect and return the answer
    answer ? () 
  }

  /*
       Classes that can be used to construct trial trees
   */
  case class L(x: Int) extends Tree[Int] {
    def isLeaf = true
    def subtrees() = ???
    def value = x
  }

  case class F(s: Seq[Tree[Int]]) extends Tree[Int] {
    def isLeaf = false
    def subtrees() = s
    def value = ???
    override def toString = s.mkString("F(", ", ", ")")
  }

  case class G(s: Tree[Int]*) extends Tree[Int] {
    def isLeaf = false
    def subtrees() = s
    def value = ???
    override def toString = s.mkString("G(", ", ", ")")
  }

  /** Depth m balanced tree with a fringe of consecutive numbers of length 2^n
    * that starts at 2^m * n
    */
  def tr(m: Int, n: Int): Tree[Int] =
    if (m == 0) L(n) else G(tr(m - 1, 2 * n), tr(m - 1, 2 * n + 1))

  /** Depth 1 tree of width n starting at m */
  def ln(m: Int, n: Int): Tree[Int] = F((m until (m + n)).map(L(_)))

  def main(args: Array[String]): Unit = {
    val t1 = tr(5, 7)
    val t2 = ln(224, 1 + 255 - 224)
    val t3 = ln(0, 1)
    val t4 = ln(0, 0)
    println("Expecting true, false, false, false")
    println(sameLeaves(t1, t2))
    println(sameLeaves(t2, t3))
    println(sameLeaves(t3, t4))
    println(sameLeaves(t4, t1))
    println("Expecting true, true, true, true")
    for (t <- List(t1, t2, t3, t4)) println(sameLeaves(t, t))
    exit()
  }

}
