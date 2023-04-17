/**
  * An uncluttered implementation of rectangular arrays, providing
  * iteration over all elements, neighbourhoods, wrap-around indexing, etc.
  *
  */

import scala.reflect.ClassTag

class RectangularArray[T](val cols: Int, val rows: Int)(implicit tag: ClassTag[T]) extends Iterable[T] {
  /** Elements in adjacent columns appear consecutively in `a` */
  private val a: Array[T] = Array.ofDim[T](rows * cols)(tag)

  override def toString(): String = s"RectangularArray($cols, $rows)"

  def print: Unit = {
    for ( r <- 0 until rows ) {
      for (c <-0 until cols ) Console.print(s"${this(c, r)}")
      Console.println("")
    }
  }

  /** Number of elements in the array */
  val elements: Int = rows * cols

  /**
    *  The `n`th element of the array: elements in adjacent columns appear consecutively
    *
    *  Pre: n<elements
    */
  @inline def apply(n: Int): T = a(n)

  /**
    * The element in column `c` of row `r`
    *
    * Pre: 0<=c<cols && o<=r<rows
    */
  @inline def apply(c: Int, r: Int): T = a(r * cols + c)

  /**
    * Update the element in column `c` of row `r`
    *
    * Pre: 0<=c<cols && o<=r<rows
    */
  @inline def update(c: Int, r: Int, t: T): Unit = {
    a(r * cols + c) = t
  }

  /**
    * As `apply`, but `c` and `r` "wrap around" modulo `cols` and `rows`
    * respectively.
    *
    * Pre: -cols <= c, -rows<=r
    */
  @inline private def wrapply(c: Int, r: Int): T = apply((c+cols)%cols,(r+rows)%rows)

  // row and column deltas for neighbours listed in conventional neighbour order,
  // namely clockwise from the north west. Any consistent cyclic permutation
  // (rotation) of both these arrays is permissible, but pointless.
  private val deltaR  = Array(-1, -1, -1,  0, +1, +1, +1, 0)
  private val deltaC  = Array(+1,  0, -1, -1, -1, 0, +1, +1)

  /** Indexes of a cell's neighbours: `0 until 8`*/
  val Neighbourhood   = RectangularArray.Neighbourhood

  /** Count of a cell's neighbours: `8` */
  val Neighbours      = RectangularArray.Neighbours

  /**
    *  Elements of the proper neighbourhood of `(c, r)` on
    *  the "toroidal" surface represented by this array.
    *  Elements appear in the conventional neighbour order; viz "clockwise
    *  from the north-west neightbour".
    */
  def neighbourhood(c: Int, r: Int): IndexedSeq[T] =
    for { loc <- Neighbourhood } yield wrapply(c + deltaC(loc), r + deltaR(loc))

  /**
    * Delivers the elements of the array such that elements in adjacent columns
    * appear consecutively.
    */
  override def iterator: Iterator[T] = new Iterator[T] {
    var i = 0
    def hasNext: Boolean = i < elements
    def next(): T = {
      val v = a(i)
      i += 1
      v
    }
  }
}

object RectangularArray {
  def apply[T](cols: Int, rows: Int)(init: (Int, Int) => T)(implicit tag: ClassTag[T]): RectangularArray[T] = {
    val a = new RectangularArray[T](cols, rows)
    for {c <- 0 until cols; r <- 0 until rows}
      a(c, r) = init(c, r)
    a
  }
  val Neighbourhood   = 0 until 8
  val Neighbours      = 8
}