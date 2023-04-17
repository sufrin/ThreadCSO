import scala.reflect.ClassTag

class ColByRowArray[T](val cols: Int, val rows: Int)(implicit tag: ClassTag[T]) extends Iterable[T] {
  val a: Array[T] = Array.ofDim[T](rows * cols)(tag)

  val elements: Int = rows * cols

  @inline def apply(n: Int): T = a(n)

  @inline def apply(c: Int, r: Int): T = a(r * cols + c)

  @inline def update(c: Int, r: Int, t: T): Unit = {
    a(r * cols + c) = t
  }

  @inline def wrapply(c: Int, r: Int): T = apply((c+cols)%cols,(r+rows)%rows)

  // row and column deltas for neighbours listed in clockwise order from
  private val nr = Array(-1, -1, -1,  0, +1, +1, +1, 0)
  private val nc = Array(+1,  0, -1, -1, -1, 0, +1, +1)
  private val ix = 0 until 8

  /** Elements in the proper neighbourhood of (c, r) */
  def neighbours(c: Int, r: Int): IndexedSeq[T] =
    for { loc <- ix } yield wrapply(c + nc(loc), r + nr(loc))

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

object ColByRowArray {
  def apply[T](cols: Int, rows: Int)(init: (Int, Int) => T)(implicit tag: ClassTag[T]): ColByRowArray[T] = {
    val a = new ColByRowArray[T](cols, rows)
    for {c <- 0 until cols; r <- 0 until rows}
      a(c, r) = init(c, r)
    a
  }
}