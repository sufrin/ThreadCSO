import io.SourceLocation.SourceLocation
import io.threadcso._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

abstract class AbstractParTest(implicit loc: SourceLocation) {

  locally { scala.util.Properties.setProp("io.threadcso.pool.REPORT", "true") }

  def withCurrentPool(expr: PROC) =
      expr.withExecutor(io.threadcso.process.CSOThreads.poolKIND)
 
  def job(args: Array[String]): Array[String]

  def apply(args: Array[String]): Array[String] = {
    var result: Array[String] = null
    run(proc("Runtime System") {} || proc("job") { result = job(args) })
    result
  }

  val shutDownThread: Thread = new Thread() {
    override def run(): Unit = {
      Console.println(s"Shutting down thread pools for test at $loc")
      io.threadcso.process.CSOThreads.shutDown()
    }
  }

  java.lang.Runtime.getRuntime.addShutdownHook(shutDownThread)

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
        for (i <- 0 until args.length) yield
            withCurrentPool {
              proc (s"proc-$i")   { result(i) = args(i) }
            }
    // The parallel composition 
    val copy = ||(copiers)
    copy()
    result
  }
}

object INTERFERETEST extends AbstractParTest {

  /**
   *   Fail to make a copy of args in parallel, an element at a time, because of interference
   */
  def job(args: Array[String]): Array[String] = {
    var result = new Array[String](args.length)
    

    // One proc for each of the args
    val copiers =
        for (i <- 0 until args.length) yield
                    withCurrentPool {        
                       proc (s"proc-$i")
                       {  
                          result(i) = args(i)
                       }
                    }

    // Interfere with the copy at a random location
    val interfere = withCurrentPool {
            proc ("interfere") {
            sleepms(Random.nextInt(1000))
            val n = Random.nextInt(args.length)
            result(n) = "INTERFERENCE"
            println(s"Interference at $n by ${Thread.currentThread}")
        }
    }
    
    // The parallel composition of the legitimate copiers
    val copy = ||(copiers)

    // augment with some concurrent interference
    (interfere || copy || interfere || interfere || interfere)()
    result
  }
}

class PoolTest extends AnyFlatSpec {

  behavior of "-- PoolTest"
  
  it should "have started and run a virtual thread" in {
    import java.lang.Thread
    Thread.startVirtualThread(() => {
       System.out.println(s"Running ${Thread.currentThread}")
    })
  }

  it should "have run some procs with different executors concurrently and reported on them" in {
     import io.threadcso.process.CSOThreads.getExecutor
     scala.util.Properties.setProp("io.threadcso.pool.REPORT", "true")

     def hello(poolKind: String) = (proc (poolKind) {
        System.out.println(s"Running $poolKind ${Thread.currentThread}");
     }).withExecutor(poolKind)

     System.out.println(s"Main thread is ${Thread.currentThread}");
     
     (hello("UNPOOLED") ||
      hello("ADAPTIVE") ||
      hello("CACHED")   ||
      hello("UNPOOLED") ||
      hello("SIZED")    ||
      hello("VIRTUAL"))()
  }

  val shutDownThread: Thread = new Thread() {
    override def run(): Unit = {
      Console.println(s"Shutting down thread pools for $this")
      io.threadcso.process.CSOThreads.shutDown()
    }
  }

  java.lang.Runtime.getRuntime.addShutdownHook(shutDownThread)

}

class ParTests extends AnyFlatSpec {

  locally { scala.util.Properties.setProp("io.threadcso.pool.REPORT", "true") }

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

  behavior of "-- PAR TEST"

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

  behavior of "-- INTERFERE TEST"
  
  it should s"have reported interference with $N vthreads and some random interference" in {
    val args = Array.fill(N)(randStr(10))
    val s = INTERFERETEST(args)
    assert (!(s == args))
  }
  
  it should s"have reported interference with $N ADAPTIVE threads and some random interference" in {
    val args = Array.fill(N)(randStr(10))
    scala.util.Properties.setProp("io.threadcso.pool.KIND", "ADAPTIVE")
    val s = INTERFERETEST(args)
    assert (!(s == args))
  }
  
  it should s"have reported interference with $N UNPOOLED threads and some random interference" in {
    val args = Array.fill(N)(randStr(10))
    scala.util.Properties.setProp("io.threadcso.pool.KIND", "UNPOOLED")
    val s = INTERFERETEST(args)
    assert (!(s == args))
  }

}
