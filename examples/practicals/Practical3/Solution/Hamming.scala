import io.threadcso._
import io.threadcso.component._
import scala.language.postfixOps

object Hamming {
  // Flush or close an input  channel
  // Flushing is the coward's way out of making the 
  def flush[T](in: ?[T]) = in.closeIn() // { repeat{ in?(); () } }

  // Tee components, giving two outputs
  def Tee[T](in: ?[T], out1: ![T], out2: ![T]) = proc {
    var v   = in.nothing
    val out = (proc(s"Tee-${out1}!") {out1!v} || proc(s"Tee-${out2}!"){out2!v})
    repeat{ v = in?(); out() }
    out1.closeOut() ; out2.closeOut(); flush(in);
  }

  // Merge two non-empty ascending streams
  def Merge(left: ?[Int], right: ?[Int], out: ![Int], id: Int) = proc("Merge") {
    // Invariant: l is last value read from left; r is last value read from right
    def Report(v:Int) = if (false) println("Merge "+id+" outputs "+v);
    var l = left?(); var r = right?();
    repeat {
      if      (l<r) { out!l; Report(l); l=left?() }
      else if (l==r){ out!l; Report(l); l=left?(); r=right?() }
      else { out!r; Report(r); r=right?() }
    }
    // Flush inputs
    flush(left)
    flush(right)
    out.closeOut()
  }

  // Send t to out, then copy from in to out
  def Prefix[T](t: T)(in: ?[T], out: ![T]) = proc { 
    out!t; repeat{ out!(in?()) }
    flush(in); out.closeOut()
  }

  // Map function f over input stream
  def Map[I,O] (f: I => O) (in: ?[I], out: ![O]) = proc {
    repeat { out!(f(in?())) }
    flush(in); out.closeOut()
  }

  /**
  The diagram below illustrates how we'll name the channels and components

  /->[Tee1]--h3-->[Tee2]--h5-->[*5]
  |    |            |           |
  h1   h2           h4          t5
  |    |            |           |
  |    V            V           |
  |   [*2]         [*3]         |
  |    |            |           |
  |    t2           t3          |
  |    |            |           |
  |    |            V           V
  |    |--------[Merge1]-m1->[Merge2]
  |                             |
  \--[Tee0]<-h0-[Prefix 1]<-m2--/  

  */

  // Put the system together per diagram, buffering each channel by buf
  def Network(out: ![Int], buf: Int) = {
    val h0 = OneOne[Int]("h0") 
    val h1 = OneOne[Int]("h1") 
    val h2 = OneOne[Int]("h2") 
    val h3 = OneOne[Int]("h3") 
    val h4 = OneOne[Int]("h4") 
    val h5 = OneOne[Int]("h5") 
    val m1 = OneOne[Int]("m1") 
    val m2 = OneOne[Int]("m2") 
    def t(name: String) = if (buf==0) OneOne[Int](name) else OneOneBuf[Int](buf, name)
    val t2 = t("t2")
    val t3 = t("t3")
    val t5 = t("t5")
    (Tee(h0, out, h1) withName "Tee0")          || 
    (Prefix(1)(m2, h0) withName "Prefix")       || 
    (Tee(h1, h2, h3) withName "Tee1")           || 
    (Map((x:Int) => 2*x)(h2,t2) withName "map (*2)") || 
    (Tee(h3, h4, h5) withName "Tee2")           || 
    (Map((x:Int) => 3*x)(h4,t3) withName "map (*3)") || 
    (Merge(t2, t3, m1, 1) withName "Merge1")    ||
    (Map((x:Int) => 5*x)(h5,t5) withName "map (*5)") || 
    (Merge(t5, m1, m2, 2) withName "Merge2")
  }

  // Print N values from report onto the console
  def Monitor(report: ?[Int], N: Int) = proc ("Monitor") {
    for (i <- 1 to N){ println(s"$i : ${report?()}")}
    report.closeIn()
  }

  def main(args: Array[String]) = { 
    val report = OneOne[Int] 
    val buf = if (args.length>0) args(0).toInt else 0
    if (args.contains("-d")) println(debugger)
    (Network(report, buf) || Monitor(report, 1000))()
  }

}

/** 

With no buffering, this deadlocks after outputting 1, 2, 3, 4, 5, 6.  (The
precise place of deadlock may vary, depending upon the order in which the Tees
do their outputs, and the order in which the Merges do their inputs.)

- Tee0 is waiting to output 6 on h1

- Prefix is waiting to output 8 on h0

- Merge2 is waiting to output 9 on m2

- map (*5) is waiting to output 15 on t5

- Merge1 is waiting to output 10 on m1

- map (*3) is waiting to input on h4 (its last output was 12)

- map (*2) is waiting to input on h2 (its last output was 10)

- Tee2 is waiting to output 4 on h5

- Tee1 is wating to output 5 on h3

With buffering of at least 224, we find the 1000th Hamming number is 51200000.

To get the network to terminate when Monitor closes report, we need to get all
components to flush their input streams, then close their output stream.

*/




