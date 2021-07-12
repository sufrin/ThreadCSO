import Labelling.Image
import io.threadcso._

object Labelling
{
  type Rectangular[T] = Array[Array[T]]

  def rows[T](arr: Rectangular[T]) = arr.size

  def cols[T](arr: Rectangular[T]) = arr(0).size

  type Image = Rectangular[Boolean]
  type Label = (Int, Int)
  type Labelling = Rectangular[Label]

  var trace, showOriginal = false

  def smaller(p1: (Int, Int), p2: (Int, Int)): Boolean =
  {
    val (r1, c1) = p1
    val (r2, c2) = p2
    r1 < r2 || (r1 == r2 && c1 < c2)
  }

  def neighbours(r: Int, c: Int, rs: Int, cs: Int): List[(Int, Int)] =
  {
    var res: List[(Int, Int)] = Nil
    if (r > 0) res = (r - 1, c) :: res
    if (r < rs - 1) res = (r + 1, c) :: res
    if (c > 0) res = (r, c - 1) :: res
    if (c < cs - 1) res = (r, c + 1) :: res
    res
  }

  // parallel algorithm
  def labelling_par(im: Image, W: Int): Labelling =
  {
    import io.threadcso._
    val rs = rows(im)
    val cs = cols(im)

    val global = Array.ofDim[Label](rs, cs)
    val publishBarrier = Barrier(W)
    val termBarrier    = new lock.AndBarrier(W)

    // This worker deals with the `local [start..end)` rows
    def worker(me: Int, start: Int, end: Int): PROC =
      proc(s"worker($me, $start, $end")
      { var round = 0
        val local = Array.ofDim[Label](rs, cs)

        // Export the boundary rows, then import the adjacent rows
        def exportAndImport() =
        { // Export boundary rows
          publishBarrier.sync()
          if (me!=0)   global(start) = local(start).clone   // First row
          if (me!=W-1) global(end-1) = local(end - 1).clone // Last row
          // Import rows adjacent to boundary
          publishBarrier.sync()
          if (me != 0)     local(start - 1) = global(start - 1) //
          if (me != W - 1) local(end)       = global(end)
          publishBarrier.sync
        }

        // initialization
        for (r <- start until end; c <- 0 until cs; if im(r)(c))
        {
          local(r)(c)  = (r, c)
        }

        var finished = false
        while (!finished)
        {
          var unchanged = true

          exportAndImport()

          for (r <- start until end)
          { for (c <- 0 until cs)
              if (im(r)(c))
              { var smallest = local(r)(c)
                for ((nr, nc) <- neighbours(r, c, rs, cs))
                {
                  if (im(nr)(nc) && smaller(local(nr)(nc), smallest))
                  {
                    smallest = local(nr)(nc)
                    unchanged = false
                  }
                }
                local(r)(c) = smallest
              }
          }

          finished = termBarrier.sync(unchanged)
          round += 1
        }
        //
        // Each worker can now publish its own rows independently
        //
        publishBarrier.sync()
        for (r <- start until end) global(r) = local(r).clone()
      }

    val heights   = fairHeights(rs, W)
    var start     = 0
    def mkWorker(me: Int): PROC =
    { val next   = start+heights(me)
      val result = worker(me, start, next)
      start = next
      result
    }

    val computation = (|| (for (me <- 0 until W) yield mkWorker(me)))
    computation()
    global
  }

  def fairHeights(rs: Int, w: Int) =
  { var extra  = rs % w
    val height = rs / w
    def getExtra() = if (extra==0) 0 else { extra -=1; 1 }
    for (i<-0 until w) yield height + getExtra()
  }

  // Some workers might be redundant
  def irredundant(rs: Int, w: Int) = fairHeights(rs, w).filter(_ != 0).size

  // overengineered sequential algorithm: not exploiting that min(S) is in S
  def labelling_seq_over(im: Image): Labelling =
  {
    val rs = rows(im)
    val cs = cols(im)
    var result, nextResult = Array.ofDim[Label](rs, cs)

    // initialise
    for (r <- 0 until rs; c <- 0 until cs; if im(r)(c))
    {
      result(r)(c)     = (r, c)
      nextResult(r)(c) = (r, c)
    }

    if (trace) printLabelling(im, result, "--- Initial state")
    var unchanged = false
    var round     = 0   // instrumentation
    while (!unchanged)
    {
      unchanged = true
      round += 1
      for (r <- 0 until rows(im))
      {
        for (c <- 0 until cols(im))
          if (im(r)(c))
          {
            var smallest = result(r)(c)
            for ((nr, nc) <- neighbours(r, c, rs, cs))
            {
              if (im(nr)(nc) && smaller(result(nr)(nc), smallest))
              {
                smallest = result(nr)(nc)
                unchanged = false
              }
            }
            nextResult(r)(c) = smallest
          }
      }

      if (!unchanged)
      {
        if (trace) printLabelling(im, nextResult, s"--- Round($round)")
      }

      {val t = result; result = nextResult; nextResult = t }
    }
    println(s"$round rounds")
    //
    result
  }

  // sequential algorithm: bottom-to top
  def labelling_seq(im: Image): Labelling =
  {
    val rs = rows(im)
    val cs = cols(im)
    var result = Array.ofDim[Label](rs, cs)

    // initialise
    for (r <- 0 until rs; c <- 0 until cs; if im(r)(c))
    {
      result(r)(c)     = (r, c)
    }

    if (trace) printLabelling(im, result, "--- Initial state")
    var unchanged = false
    var round     = 0   // instrumentation
    while (!unchanged)
    {
      unchanged = true
      round += 1
      for (rx <- 0 until rs)
      { val r = rs-rx-1
        for (cx <- 0 until cs)
        { val c = cs - cx -1
          if (im(r)(c))
          {
            var smallest = result(r)(c)
            for ((nr, nc) <- neighbours(r, c, rs, cs))
            {
              if (im(nr)(nc) && smaller(result(nr)(nc), smallest))
              {
                smallest = result(nr)(nc)
                unchanged = false
              }
            }
            result(r)(c) = smallest
          }
        }
      }

      if (!unchanged)
      {
        if (trace) printLabelling(im, result, s"--- Round($round)")
      }

    }
    println(s"$round rounds")
    //
    result
  }




  //
  def printLabelling(im: Image, labelling: Labelling, caption: String = null): Unit =
  {
    println(if (caption==null) s"--- (${rows(im)}x${cols(im)})" else caption)
    var count = 0
    val map = new scala.collection.mutable.HashMap[Label, Int]()

    // pre-pass: assign integers to labels
    for (r <- 0 until rows(labelling))
      for (c <- 0 until cols(labelling))
        if (im(r)(c) && !map.isDefinedAt(labelling(r)(c)))
        {
          map += ((labelling(r)(c), count))
          count = count + 1
        }

    def letter(n: Int): Char = {
      val code =
          if (count < 10) '0'+n else
          if (count < 26) 'a'+n else
          if (count < 52) (if (n<26) 'a'+n else 'A'+n-26) else
          if (count < 62) (if (n<10) '0'+n else if (n<36) 'a'+n-10 else 'A'+n-36)
          else (' '+n+1)
      code.toChar
    }

    // second pass
    for (r <- 0 until rows(labelling))
    {
      for (c <- 0 until cols(labelling))
      {
        val lab = labelling(r)(c)
        if (map.isDefinedAt(lab))
          print(letter(map(lab)))
        else
          print(" ")
      }
      println('|')
    }
    println(s"--- $count Regions")
  }

  def printImage(im: Image): Unit =
  {
    for (r <- 0 until rows(im))
    {
      for (c <- 0 until cols(im)) print(if (im(r)(c)) "*" else " ")
      println("|")
    }
  }

  // Reads an image from its specification as a string
  // ~~An image spec is a sequence of row specs
  // ~~~~~separated by newlines.
  // ~~A row spec is a sequence of pixel specs
  // ~~White pixels are specified by spaces
  // ~~Black pixels are specified by non-spaces

  def readImage(in: String): Image =
  { val rows  = in.dropWhile(_ == '\n').split('\n')
    val width = rows.map(_.length).max

    def mkRow(s: String) = s.padTo(width, ' ').map(_ != ' ').toArray

    rows.map(mkRow).toArray
  }

  def fileImage(path: String): Image = readImage(scala.io.Source.fromFile(path).mkString)

  val eg0 =
    """
*** **
*   *
    ** """

  val eg1 =
    """
*** ** ****
*   *  ***
*****  *

    **"""

  val eg1a=
    """
      ******
           *
      ***  *
      *    *
      ******
    """

  val eg2 =
    """
                            ********
                                  ******************
                               ************************
                             ****************************

           *******         ***************************            *******  *
           *******         *****************************          *******  *
           *******         ******************************         *******  *
           *******         *************     **************       *******  *
           *******        ************          ***********       *******
           *******         **********             ***********     *******
           *******        **********************************     *******
           *******     **********                *********        *******
           *******   *********                  *********         *******
"""

  val eg3 =
    """
                            ********
                                  ******************
                               ************************
                             ****************************

                        **************************************
                      *****************        *****************
                     **************                **************
                    ************                      ************
                   ***********                          ***********
                  ***********                            ***********
                 ****************************************************
                 *********                                  *********

           *******         ***************************            **********
           *******         *****************************          *******  *
           *******         ******************************         *******  *
           *******         *************     **************       **********
           *******        ************          ***********       *******
           *******         **********             ***********     *******
           *******        **********               **********     *******
           *******     **********                *********        *******
           *******   *********                  *********         *******
"""


  val im0 = readImage(eg0)
  val im1 = readImage(eg1)
  val im2 = readImage(eg2)
  val im3 = readImage(eg3)

  val eg4 = (
    """
  aaa   b
c a  dd     e
 f   d     g  h
     dd      i
      d  jj
        k     l
   m  n    o  l
p     n q oo  l
 r    n     s
     t u vv
""")

  val im4 = readImage(eg4)

  val eg5 = (
    """
aa    b  c
a d     cc  e f
   g  h
 ii jj   kk
  i    l kk
      m  kk
 n  o m   k pp
nn   mm    ppp
  q     r s
t         s
""")

  val im5 = readImage(eg5)

  val im1a = readImage(eg1a)

  val images =  Array(im0, im1, im2, im3, im4, im5)

  def randomImage(rs: Int, cs: Int, prob: Double = 0.6): Image =
  {
    val im: Image = Array.ofDim(rs, cs)
    for (r<-0 until rs; c<-0 until cs) im(r)(c) =  Math.random()>prob
    im
  }

  def testImage(im: Image, W: Int): Unit =
  {
    val s = labelling_seq(im)
    if (showOriginal) printImage(im)
    printLabelling(im, s, "Sequential")
    if (W!=1) {
      // avoid using redundant
      val WW = irredundant(rows(im), W)
      val p  = labelling_par(im, WW)
      printLabelling(im, p, s"Parallel ($WW workers used out of $W requested)")
    }
    else
      println(s"Barriers need W>1")
  }

  def main(args: Array[String]): Unit =
  {  var W = 2
     for (arg <- args)
         arg match {
           case "-2" => W=2
           case "-3" => W=3
           case "-4" => W=4
           case "-5" => W=5
           case "-6" => W=6
           case "-7" => W=7
           case "-8" => W=8
           case "-t" => trace = false
           case "+t" => trace = true
           case "+o" => showOriginal = true
           case "0" | "1" | "2" | "3" | "4" | "5" => testImage(images(arg.toInt), W)
           case "1a" => testImage(im1a, W)
           case "-r" => testImage(randomImage(24, 15), W)
           case file => testImage(fileImage(file), W)
         }
    exit()
  }

}