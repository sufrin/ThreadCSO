import io.SourceLocation.SourceLocation
import io.threadcso._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

abstract class AbstractParTest(implicit loc: SourceLocation) {

  val shutDown: Thread = new Thread() {
    override def run(): Unit = { Console.println("Shut down") }
  }

  def job(args: Array[String]): Array[String]

  def apply(args: Array[String]): Array[String] = {
    var result: Array[String] = null
    run(proc("Runtime System") {} || proc("job") { result = job(args) })
    result
  }

  java.lang.Runtime.getRuntime.addShutdownHook(shutDown)

  println(s"Runnable $loc")
  println(debugger)
}

object PARTEST extends AbstractParTest {

  /**
   *   Make a copy of args in parallel, an element at a time
   */
  def job(args: Array[String]): Array[String] = {
    var result = new Array[String](args.length)

    // One proc for each of the args
    val copiers =
        for (i <- 0 until args.length) yield proc (s"proc-$i")   { result(i) = args(i) }
    // The parallel composition 
    val copy = ||(copiers)
    copy()
    result
  }
}

object FAILTEST extends AbstractParTest {

  /**
   *   Fail to make a copy of args in parallel, an element at a time, because of interference
   */
  def job(args: Array[String]): Array[String] = {
    var result = new Array[String](args.length)

    // One proc for each of the args
    val copiers =
        for (i <- 0 until args.length) yield proc (s"proc-$i")
        {  
           result(i) = args(i)
        }

    // Interfere with the copy at a random location
    val interfere = proc ("interfere") {
        sleep(Random.nextInt(1000))
        val n = Random.nextInt(args.length)
        result(n) = "INTERFERENCE"
        println(s"Interference at $n")
    }
    
    // The parallel composition of the copiers
    val copy = ||(copiers)
    // Copy, but with a couple of concurrent interferences
    (copy || interfere || interfere)()
    result
  }
}

class ParTests extends AnyFlatSpec {

  private val N = 1500

  private val alpha =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

  protected def randStr(M: Int) = {
    val n = Random.nextInt(M) + 2
    (1 to n).map(_ => alpha(Random.nextInt(alpha.length))).mkString
  }

  def same(a: Array[String], b: Array[String]): Boolean =
  { var at=0
    if (a.length==b.length)
    {
       while (at<a.length && a(at)==b(at)) at+=1
       at==a.length
    }
    else
       false       
  }

  it should "run a virtual thread" in {
    import java.lang.Thread
    Thread.startVirtualThread(() => { System.out.println("Hello World"); });
  }

  it should s"copy properly with $N vthreads" in {
    val args = Array.fill(N)(randStr(10))
    val s = PARTEST(args)
    assert (s === args)
  }

  it should s"copy properly with $N ADAPTIVE-pooled kthreads" in {
    val args = Array.fill(N)(randStr(10))
    scala.util.Properties.setProp("io.threadcso.pool.KIND", "ADAPTIVE")
    val s = PARTEST(args)
    assert (s === args)
  }
  
  it should s"copy properly with $N UNPOOLED-pooled kthreads" in {
    val args = Array.fill(N)(randStr(10))
    scala.util.Properties.setProp("io.threadcso.pool.KIND", "UNPOOLED")
    val s = PARTEST(args)
    assert (s === args)
  }

  it should s"not copy properly with 5 vthreads || interference" in {
    val args = Array.fill(5)(randStr(10))
    val s = FAILTEST(args)
    assert (!(s == args))
  }

}
