

import java.util.concurrent.atomic.AtomicBoolean

import io.threadcso._
import io.threadcso.lock._
import io.threadcso.semaphore._

/**
  * A primitive harness for benchmarking the performance of classes of semaphore.
  *
  * Each trial of a semaphore runs `N` processes in parallel, and has them contend for a lock (constructed
  * from the semaphore in the obvious way) around a simple incrementation invoked `TARGET/N` times in
  * each process. Elapsed time is measured, and the time-per-invocation of the lock is inferred. A perfunctory
  * check that the lock is actually acting as a lock is performed.
  *
  * The method is not entirely satisfactory, because the body of the locked code does almost no computation.
  */
class MutexTrial(val TARGET: Int, N: Int, lock: Lock)
{
  def main(args: Array[String])
  {
    val times = TARGET / N
    var n = 0
    val trial =
      ||(for (i <- 0 until N) yield
        {
          proc(i.toString)
          {
            for (j <- 0 until times)
            {
              lock withLock { n = n + 1 }
            }
          }
        }
        )
    val now = nanoTime
    trial()
    val elapsed = nanoTime - now
    print(f"${elapsed / (N * times) }%06.1f ns/inv $lock%-20s: ${N} threads ${times} times ${elapsed / 1000000 * 1e-3 }%3.3fs")
    if (n != TARGET) println(s" FAIL (${TARGET - n })")
    else println()

  }
}

/**
  * Stock CSO boolean semaphore
  * @param spin -- the number of cycles this semaphore uses a spinlock for before descheduling
  */

sealed class BooleanLock(spin: Int=500) extends
  BooleanSemaphore(true, name=null, false, parent=null, spin=spin) with Lock
{ @inline final def lock() = acquire()
  @inline final def unlock() = release()
  override def toString = s"Boolean($spin)"
}

/**
  * Stock CSO counting lock masquerading as a boolean semaphore.
  */
sealed class CountingLock(spin: Int) extends CountingSemaphore(1, null, false, null, spin=spin) with Lock
{
  @inline final def lock() = acquire()
  @inline final def unlock() = release()
  override def toString = s"Counting($spin)"
}

/**
  * A (possibly fair) Reentrant lock from `java.util.concurrent.locks.ReentrantLock(fair)`
  */
sealed class LeaLock(fair: Boolean=false)
  extends java.util.concurrent.locks.ReentrantLock(fair)
    with Lock
{
  override def toString = s"""${if (fair) "Fair " else ""}Reentrant"""
}

/** Maintaining the local state of the lock
  **/
sealed class JavaLock extends Lock
{ private [this] var available = true
  override def toString = s"JavaLock"
  @inline final def lock() = synchronized
  {  while (!available) wait(); available = false }

  @inline final def unlock() = synchronized
  {  available = true; notify() }

}

/**
  * A straighforward lock that uses the inbuilt `synchronized` method to implement `withLock`.
  */
sealed class JavaSync extends Lock
{ private [this] var available = true
  override def toString = s"JavaSync"

  override def withLock[T](body: =>T): T = synchronized { body }

  @inline final def lock()  = synchronized  {  while (!available) wait() }

  @inline final def unlock() = synchronized {  available = true; notify() }

}

/**
  * A spinlock using an atomic boolean
  */
sealed class SpinLock extends Lock {
  private [this] var w = 0l
  override def toString = s"SpinLock($w)"

  val available = new AtomicBoolean(true)

  @inline final def lock() =
  {  if (!available.getAndSet(false))
        while (!available.compareAndSet(true, false)) { w += 1 } }

  @inline final def unlock() =
  {  available.set(true) }
}



object J5   extends MutexTrial(1000000, 5, new JavaLock)
object J50  extends MutexTrial(1000000, 50, new JavaLock)
object J500 extends MutexTrial(1000000, 500, new JavaLock)
object JS5   extends MutexTrial(1000000, 5, new JavaSync)
object JS50  extends MutexTrial(1000000, 50, new JavaSync)
object JS500 extends MutexTrial(1000000, 500, new JavaSync)
object F5   extends MutexTrial(1000000, 5, new LeaLock(true))
object F50  extends MutexTrial(1000000, 50, new LeaLock(true))
object F500 extends MutexTrial(1000000, 500, new LeaLock(true))
object L5   extends MutexTrial(1000000, 5, new LeaLock)
object L50  extends MutexTrial(1000000, 50, new LeaLock)
object L500 extends MutexTrial(1000000, 500, new LeaLock)
object S5   extends MutexTrial(1000000, 5, new SpinLock)
object S50  extends MutexTrial(1000000, 50, new SpinLock)
object S500 extends MutexTrial(1000000, 500, new SpinLock)
object B5   extends MutexTrial(1000000, 5, new BooleanLock)
object B50  extends MutexTrial(1000000, 50, new BooleanLock)
object B500 extends MutexTrial(1000000, 500, new BooleanLock)
object C5   extends MutexTrial(1000000, 5, new CountingLock(5))
object C50  extends MutexTrial(1000000, 50, new CountingLock(5))
object C500 extends MutexTrial(1000000, 500, new CountingLock(5))


object MutexBenchmark {
  def main(args: Array[String]) {
      if (args.contains("-all")) {
        val trials: Seq[MutexTrial] = List(
          B5, B50, B500,
          S5, S50, S500,
          JS5, JS50, JS500,
          J5, J50, J500,
          F5, F50, F500,
          L5, L50, L500
        )
        for (i <- 0 until 5) for (trial <- trials) trial.main(args)
      }
      else
      { val trials: Seq[MutexTrial] = List (
        new MutexTrial(1000000, 5, new BooleanLock(1)),
        new MutexTrial(1000000, 5, new BooleanLock(5)),
        new MutexTrial(1000000, 5, new BooleanLock(50)),
        new MutexTrial(1000000, 5, new CountingLock(1)),
        new MutexTrial(1000000, 5, new CountingLock(5)),
        new MutexTrial(1000000, 5, new CountingLock(50)),
        new MutexTrial(1000000, 5, new BooleanLock(500)),
        new MutexTrial(1000000, 5, new BooleanLock(1500)),
        new MutexTrial(1000000, 5, new BooleanLock(2500)),
        L5,
        new MutexTrial(1000000, 50, new BooleanLock(1)),
        new MutexTrial(1000000, 50, new BooleanLock(5)),
        new MutexTrial(1000000, 50, new BooleanLock(50)),
        new MutexTrial(1000000, 50, new CountingLock(1)),
        new MutexTrial(1000000, 50, new CountingLock(5)),
        new MutexTrial(1000000, 50, new CountingLock(50)),
        new MutexTrial(1000000, 50, new BooleanLock(500)),
        new MutexTrial(1000000, 50, new BooleanLock(1500)),
        new MutexTrial(1000000, 50, new BooleanLock(2500)),
        L50,
        new MutexTrial(1000000, 500, new BooleanLock(1)),
        new MutexTrial(1000000, 500, new BooleanLock(5)),
        new MutexTrial(1000000, 500, new BooleanLock(50)),
        new MutexTrial(1000000, 500, new CountingLock(1)),
        new MutexTrial(1000000, 500, new CountingLock(5)),
        new MutexTrial(1000000, 500, new CountingLock(50)),
        new MutexTrial(1000000, 500, new BooleanLock(500)),
        new MutexTrial(1000000, 500, new BooleanLock(1500)),
        new MutexTrial(1000000, 500, new BooleanLock(2500)),
        L500
        )
        for (i <- 0 until 5) for (trial <- trials) trial.main(args)
      }

    exit()
  }
}

/* 2017 Benchmarks

Mimi:

$ cso MutexBenchmark -all | sort -n
0043.0 ns/inv JavaSync            : 5 threads 200000 times 0.043s
0059.0 ns/inv JavaSync            : 5 threads 200000 times 0.059s
0060.0 ns/inv JavaSync            : 50 threads 20000 times 0.060s
0061.0 ns/inv Reentrant           : 5 threads 200000 times 0.061s
0063.0 ns/inv JavaSync            : 5 threads 200000 times 0.063s
0063.0 ns/inv Reentrant           : 500 threads 2000 times 0.063s
0066.0 ns/inv Reentrant           : 5 threads 200000 times 0.066s
0066.0 ns/inv Reentrant           : 50 threads 20000 times 0.066s
0066.0 ns/inv Reentrant           : 500 threads 2000 times 0.066s
0067.0 ns/inv Reentrant           : 50 threads 20000 times 0.067s
0067.0 ns/inv Reentrant           : 500 threads 2000 times 0.067s
0068.0 ns/inv JavaSync            : 5 threads 200000 times 0.068s
0068.0 ns/inv JavaSync            : 500 threads 2000 times 0.068s
0068.0 ns/inv Reentrant           : 5 threads 200000 times 0.068s
0068.0 ns/inv Reentrant           : 50 threads 20000 times 0.068s
0068.0 ns/inv Reentrant           : 500 threads 2000 times 0.068s
0070.0 ns/inv JavaSync            : 50 threads 20000 times 0.070s
0071.0 ns/inv JavaSync            : 50 threads 20000 times 0.071s
0071.0 ns/inv Reentrant           : 50 threads 20000 times 0.071s
0071.0 ns/inv Reentrant           : 50 threads 20000 times 0.071s
0072.0 ns/inv Reentrant           : 5 threads 200000 times 0.072s
0072.0 ns/inv Reentrant           : 500 threads 2000 times 0.072s
0073.0 ns/inv JavaSync            : 50 threads 20000 times 0.073s
0073.0 ns/inv Reentrant           : 5 threads 200000 times 0.073s
0076.0 ns/inv JavaSync            : 5 threads 200000 times 0.076s
0077.0 ns/inv JavaSync            : 500 threads 2000 times 0.077s
0078.0 ns/inv JavaSync            : 500 threads 2000 times 0.078s
0079.0 ns/inv JavaSync            : 500 threads 2000 times 0.079s
0084.0 ns/inv JavaSync            : 500 threads 2000 times 0.084s
0090.0 ns/inv JavaSync            : 50 threads 20000 times 0.090s
0119.0 ns/inv Boolean(500)        : 50 threads 20000 times 0.119s
0127.0 ns/inv Boolean(500)        : 500 threads 2000 times 0.127s
0140.0 ns/inv Boolean(500)        : 50 threads 20000 times 0.140s
0141.0 ns/inv Boolean(500)        : 500 threads 2000 times 0.141s
0145.0 ns/inv Boolean(500)        : 50 threads 20000 times 0.145s
0149.0 ns/inv Boolean(500)        : 50 threads 20000 times 0.149s
0151.0 ns/inv Boolean(500)        : 5 threads 200000 times 0.151s
0154.0 ns/inv Boolean(500)        : 5 threads 200000 times 0.154s
0155.0 ns/inv Boolean(500)        : 500 threads 2000 times 0.155s
0156.0 ns/inv Boolean(500)        : 5 threads 200000 times 0.156s
0156.0 ns/inv Boolean(500)        : 50 threads 20000 times 0.156s
0157.0 ns/inv Boolean(500)        : 5 threads 200000 times 0.157s
0157.0 ns/inv Boolean(500)        : 500 threads 2000 times 0.157s
0162.0 ns/inv Boolean(500)        : 5 threads 200000 times 0.162s
0178.0 ns/inv SpinLock(1291750)   : 5 threads 200000 times 0.178s
0193.0 ns/inv SpinLock(5771771)   : 50 threads 20000 times 0.193s
0194.0 ns/inv SpinLock(6892464)   : 50 threads 20000 times 0.194s
0212.0 ns/inv SpinLock(3694248)   : 5 threads 200000 times 0.212s
0214.0 ns/inv JavaLock            : 5 threads 200000 times 0.214s
0225.0 ns/inv SpinLock(6051059)   : 5 threads 200000 times 0.225s
0252.0 ns/inv SpinLock(4988644)   : 5 threads 200000 times 0.252s
0256.0 ns/inv SpinLock(2649414)   : 5 threads 200000 times 0.256s
0276.0 ns/inv JavaLock            : 5 threads 200000 times 0.276s
0301.0 ns/inv JavaLock            : 5 threads 200000 times 0.301s
0319.0 ns/inv Boolean(500)        : 500 threads 2000 times 0.319s
0331.0 ns/inv JavaLock            : 5 threads 200000 times 0.331s
0333.0 ns/inv JavaLock            : 5 threads 200000 times 0.333s
0566.0 ns/inv SpinLock(4549500)   : 50 threads 20000 times 0.566s
0653.0 ns/inv SpinLock(26590415)  : 50 threads 20000 times 0.653s
0922.0 ns/inv JavaLock            : 50 threads 20000 times 0.922s
1016.0 ns/inv JavaLock            : 50 threads 20000 times 1.016s
1017.0 ns/inv SpinLock(86757805)  : 500 threads 2000 times 1.017s
1059.0 ns/inv JavaLock            : 50 threads 20000 times 1.059s
1286.0 ns/inv JavaLock            : 50 threads 20000 times 1.286s
1358.0 ns/inv SpinLock(20614774)  : 50 threads 20000 times 1.358s
1368.0 ns/inv JavaLock            : 50 threads 20000 times 1.368s
1422.0 ns/inv SpinLock(12332093)  : 500 threads 2000 times 1.422s
2107.0 ns/inv SpinLock(103910653) : 500 threads 2000 times 2.107s
2822.0 ns/inv SpinLock(79000851)  : 500 threads 2000 times 2.822s
5103.0 ns/inv SpinLock(55533561)  : 500 threads 2000 times 5.103s
5773.0 ns/inv Fair Reentrant      : 50 threads 20000 times 5.773s
5775.0 ns/inv Fair Reentrant      : 50 threads 20000 times 5.775s
5810.0 ns/inv Fair Reentrant      : 50 threads 20000 times 5.810s
5855.0 ns/inv Fair Reentrant      : 50 threads 20000 times 5.855s
5860.0 ns/inv Fair Reentrant      : 50 threads 20000 times 5.860s
6220.0 ns/inv Fair Reentrant      : 5 threads 200000 times 6.220s
6280.0 ns/inv Fair Reentrant      : 5 threads 200000 times 6.280s
6299.0 ns/inv Fair Reentrant      : 5 threads 200000 times 6.299s
6311.0 ns/inv Fair Reentrant      : 5 threads 200000 times 6.311s
6380.0 ns/inv Fair Reentrant      : 5 threads 200000 times 6.380s
9121.0 ns/inv Fair Reentrant      : 500 threads 2000 times 9.121s
9323.0 ns/inv Fair Reentrant      : 500 threads 2000 times 9.323s
9373.0 ns/inv Fair Reentrant      : 500 threads 2000 times 9.373s
9419.0 ns/inv Fair Reentrant      : 500 threads 2000 times 9.419s
9427.0 ns/inv Fair Reentrant      : 500 threads 2000 times 9.427s
12372.0 ns/inv JavaLock            : 500 threads 2000 times 12.372s
13093.0 ns/inv JavaLock            : 500 threads 2000 times 13.093s
14967.0 ns/inv JavaLock            : 500 threads 2000 times 14.967s
15670.0 ns/inv JavaLock            : 500 threads 2000 times 15.670s
16533.0 ns/inv JavaLock            : 500 threads 2000 times 16.533s

===
Boolean and Counting locks vs (non-fair) Reentrant locks.
===
$ cso MutexBenchmark | sort -n
0042.0 ns/inv Counting(1)         : 5 threads 200000 times 0.042s
0045.0 ns/inv Boolean(5)          : 5 threads 200000 times 0.045s
0048.0 ns/inv Boolean(1)          : 5 threads 200000 times 0.048s
0053.0 ns/inv Boolean(1)          : 5 threads 200000 times 0.053s
0054.0 ns/inv Counting(1)         : 5 threads 200000 times 0.054s
0056.0 ns/inv Boolean(1)          : 5 threads 200000 times 0.056s
0056.0 ns/inv Counting(1)         : 5 threads 200000 times 0.056s
0057.0 ns/inv Reentrant           : 5 threads 200000 times 0.057s
0059.0 ns/inv Counting(1)         : 5 threads 200000 times 0.059s
0060.0 ns/inv Reentrant           : 5 threads 200000 times 0.060s
0065.0 ns/inv Reentrant           : 5 threads 200000 times 0.065s
0066.0 ns/inv Boolean(5)          : 5 threads 200000 times 0.066s
0066.0 ns/inv Counting(1)         : 50 threads 20000 times 0.066s
0067.0 ns/inv Boolean(1)          : 50 threads 20000 times 0.067s
0067.0 ns/inv Counting(1)         : 50 threads 20000 times 0.067s
0067.0 ns/inv Counting(1)         : 50 threads 20000 times 0.067s
0073.0 ns/inv Reentrant           : 5 threads 200000 times 0.073s
0074.0 ns/inv Boolean(1)          : 50 threads 20000 times 0.074s
0074.0 ns/inv Reentrant           : 500 threads 2000 times 0.074s
0075.0 ns/inv Boolean(1)          : 5 threads 200000 times 0.075s
0075.0 ns/inv Reentrant           : 500 threads 2000 times 0.075s
0076.0 ns/inv Reentrant           : 5 threads 200000 times 0.076s
0077.0 ns/inv Counting(1)         : 500 threads 2000 times 0.077s
0079.0 ns/inv Reentrant           : 500 threads 2000 times 0.079s
0082.0 ns/inv Reentrant           : 500 threads 2000 times 0.082s
0083.0 ns/inv Reentrant           : 50 threads 20000 times 0.083s
0084.0 ns/inv Reentrant           : 50 threads 20000 times 0.084s
0085.0 ns/inv Counting(1)         : 500 threads 2000 times 0.085s
0086.0 ns/inv Reentrant           : 50 threads 20000 times 0.086s
0089.0 ns/inv Counting(1)         : 500 threads 2000 times 0.089s
0090.0 ns/inv Boolean(5)          : 5 threads 200000 times 0.090s
0090.0 ns/inv Counting(1)         : 50 threads 20000 times 0.090s
0092.0 ns/inv Reentrant           : 50 threads 20000 times 0.092s
0093.0 ns/inv Reentrant           : 500 threads 2000 times 0.093s
0096.0 ns/inv Counting(1)         : 500 threads 2000 times 0.096s
0096.0 ns/inv Reentrant           : 50 threads 20000 times 0.096s
0099.0 ns/inv Boolean(5)          : 5 threads 200000 times 0.099s
0100.0 ns/inv Boolean(1)          : 5 threads 200000 times 0.100s
0101.0 ns/inv Boolean(5)          : 5 threads 200000 times 0.101s
0102.0 ns/inv Boolean(5)          : 50 threads 20000 times 0.102s
0103.0 ns/inv Counting(1)         : 50 threads 20000 times 0.103s
0105.0 ns/inv Boolean(5)          : 50 threads 20000 times 0.105s
0108.0 ns/inv Boolean(5)          : 50 threads 20000 times 0.108s
0108.0 ns/inv Counting(1)         : 500 threads 2000 times 0.108s
0108.0 ns/inv Counting(5)         : 5 threads 200000 times 0.108s
0111.0 ns/inv Boolean(1)          : 50 threads 20000 times 0.111s
0112.0 ns/inv Boolean(5)          : 50 threads 20000 times 0.112s
0112.0 ns/inv Counting(5)         : 5 threads 200000 times 0.112s
0116.0 ns/inv Counting(5)         : 500 threads 2000 times 0.116s
0118.0 ns/inv Boolean(50)         : 5 threads 200000 times 0.118s
0118.0 ns/inv Boolean(50)         : 500 threads 2000 times 0.118s
0119.0 ns/inv Counting(5)         : 5 threads 200000 times 0.119s
0124.0 ns/inv Boolean(5)          : 500 threads 2000 times 0.124s
0124.0 ns/inv Boolean(50)         : 500 threads 2000 times 0.124s
0125.0 ns/inv Boolean(1)          : 500 threads 2000 times 0.125s
0125.0 ns/inv Counting(5)         : 5 threads 200000 times 0.125s
0126.0 ns/inv Counting(5)         : 50 threads 20000 times 0.126s
0126.0 ns/inv Counting(5)         : 50 threads 20000 times 0.126s
0127.0 ns/inv Counting(5)         : 500 threads 2000 times 0.127s
0129.0 ns/inv Counting(5)         : 500 threads 2000 times 0.129s
0130.0 ns/inv Boolean(2500)       : 500 threads 2000 times 0.130s
0131.0 ns/inv Counting(5)         : 50 threads 20000 times 0.131s
0132.0 ns/inv Boolean(50)         : 500 threads 2000 times 0.132s
0133.0 ns/inv Boolean(1500)       : 500 threads 2000 times 0.133s
0133.0 ns/inv Boolean(2500)       : 500 threads 2000 times 0.133s
0137.0 ns/inv Boolean(50)         : 500 threads 2000 times 0.137s
0138.0 ns/inv Counting(5)         : 50 threads 20000 times 0.138s
0139.0 ns/inv Boolean(500)        : 50 threads 20000 times 0.139s
0140.0 ns/inv Boolean(50)         : 500 threads 2000 times 0.140s
0145.0 ns/inv Boolean(50)         : 5 threads 200000 times 0.145s
0145.0 ns/inv Boolean(50)         : 50 threads 20000 times 0.145s
0145.0 ns/inv Counting(5)         : 50 threads 20000 times 0.145s
0146.0 ns/inv Boolean(2500)       : 50 threads 20000 times 0.146s
0147.0 ns/inv Boolean(1500)       : 50 threads 20000 times 0.147s
0147.0 ns/inv Boolean(2500)       : 500 threads 2000 times 0.147s
0149.0 ns/inv Boolean(500)        : 5 threads 200000 times 0.149s
0150.0 ns/inv Counting(5)         : 5 threads 200000 times 0.150s
0151.0 ns/inv Boolean(50)         : 50 threads 20000 times 0.151s
0151.0 ns/inv Counting(5)         : 500 threads 2000 times 0.151s
0153.0 ns/inv Boolean(1500)       : 5 threads 200000 times 0.153s
0154.0 ns/inv Boolean(1)          : 50 threads 20000 times 0.154s
0154.0 ns/inv Boolean(500)        : 500 threads 2000 times 0.154s
0154.0 ns/inv Boolean(500)        : 500 threads 2000 times 0.154s
0155.0 ns/inv Boolean(1500)       : 50 threads 20000 times 0.155s
0157.0 ns/inv Boolean(1500)       : 500 threads 2000 times 0.157s
0159.0 ns/inv Boolean(1500)       : 5 threads 200000 times 0.159s
0159.0 ns/inv Boolean(50)         : 5 threads 200000 times 0.159s
0160.0 ns/inv Boolean(1500)       : 500 threads 2000 times 0.160s
0160.0 ns/inv Boolean(2500)       : 5 threads 200000 times 0.160s
0160.0 ns/inv Boolean(5)          : 50 threads 20000 times 0.160s
0161.0 ns/inv Boolean(50)         : 50 threads 20000 times 0.161s
0161.0 ns/inv Boolean(500)        : 50 threads 20000 times 0.161s
0162.0 ns/inv Boolean(2500)       : 5 threads 200000 times 0.162s
0162.0 ns/inv Boolean(5)          : 500 threads 2000 times 0.162s
0162.0 ns/inv Boolean(500)        : 5 threads 200000 times 0.162s
0163.0 ns/inv Boolean(1500)       : 5 threads 200000 times 0.163s
0163.0 ns/inv Boolean(50)         : 5 threads 200000 times 0.163s
0163.0 ns/inv Boolean(500)        : 5 threads 200000 times 0.163s
0163.0 ns/inv Boolean(500)        : 5 threads 200000 times 0.163s
0166.0 ns/inv Boolean(2500)       : 5 threads 200000 times 0.166s
0167.0 ns/inv Boolean(1500)       : 50 threads 20000 times 0.167s
0167.0 ns/inv Boolean(2500)       : 5 threads 200000 times 0.167s
0167.0 ns/inv Boolean(2500)       : 5 threads 200000 times 0.167s
0167.0 ns/inv Boolean(50)         : 5 threads 200000 times 0.167s
0167.0 ns/inv Boolean(50)         : 50 threads 20000 times 0.167s
0167.0 ns/inv Boolean(50)         : 50 threads 20000 times 0.167s
0167.0 ns/inv Boolean(500)        : 5 threads 200000 times 0.167s
0168.0 ns/inv Boolean(1)          : 50 threads 20000 times 0.168s
0170.0 ns/inv Boolean(1500)       : 5 threads 200000 times 0.170s
0171.0 ns/inv Boolean(1500)       : 5 threads 200000 times 0.171s
0171.0 ns/inv Boolean(1500)       : 50 threads 20000 times 0.171s
0174.0 ns/inv Boolean(5)          : 500 threads 2000 times 0.174s
0175.0 ns/inv Boolean(5)          : 500 threads 2000 times 0.175s
0176.0 ns/inv Boolean(500)        : 500 threads 2000 times 0.176s
0177.0 ns/inv Counting(5)         : 500 threads 2000 times 0.177s
0180.0 ns/inv Boolean(1)          : 500 threads 2000 times 0.180s
0181.0 ns/inv Boolean(500)        : 50 threads 20000 times 0.181s
0182.0 ns/inv Boolean(2500)       : 50 threads 20000 times 0.182s
0187.0 ns/inv Boolean(5)          : 500 threads 2000 times 0.187s
0188.0 ns/inv Counting(50)        : 50 threads 20000 times 0.188s
0189.0 ns/inv Boolean(2500)       : 500 threads 2000 times 0.189s
0189.0 ns/inv Counting(50)        : 5 threads 200000 times 0.189s
0194.0 ns/inv Boolean(2500)       : 50 threads 20000 times 0.194s
0194.0 ns/inv Counting(50)        : 500 threads 2000 times 0.194s
0195.0 ns/inv Boolean(500)        : 500 threads 2000 times 0.195s
0196.0 ns/inv Boolean(500)        : 50 threads 20000 times 0.196s
0198.0 ns/inv Counting(1)         : 5 threads 200000 times 0.198s
0199.0 ns/inv Boolean(2500)       : 50 threads 20000 times 0.199s
0202.0 ns/inv Boolean(500)        : 50 threads 20000 times 0.202s
0206.0 ns/inv Boolean(1)          : 500 threads 2000 times 0.206s
0213.0 ns/inv Boolean(1500)       : 50 threads 20000 times 0.213s
0214.0 ns/inv Boolean(2500)       : 50 threads 20000 times 0.214s
0223.0 ns/inv Counting(50)        : 500 threads 2000 times 0.223s
0225.0 ns/inv Counting(50)        : 500 threads 2000 times 0.225s
0240.0 ns/inv Boolean(1)          : 500 threads 2000 times 0.240s
0247.0 ns/inv Counting(50)        : 50 threads 20000 times 0.247s
0248.0 ns/inv Counting(50)        : 500 threads 2000 times 0.248s
0252.0 ns/inv Counting(50)        : 5 threads 200000 times 0.252s
0256.0 ns/inv Counting(50)        : 50 threads 20000 times 0.256s
0258.0 ns/inv Counting(50)        : 50 threads 20000 times 0.258s
0260.0 ns/inv Counting(50)        : 5 threads 200000 times 0.260s
0261.0 ns/inv Counting(50)        : 5 threads 200000 times 0.261s
0263.0 ns/inv Counting(50)        : 5 threads 200000 times 0.263s
0270.0 ns/inv Counting(50)        : 50 threads 20000 times 0.270s
0271.0 ns/inv Boolean(1)          : 500 threads 2000 times 0.271s
0330.0 ns/inv Counting(50)        : 500 threads 2000 times 0.330s
0339.0 ns/inv Boolean(1500)       : 500 threads 2000 times 0.339s
0352.0 ns/inv Boolean(500)        : 500 threads 2000 times 0.352s
0428.0 ns/inv Boolean(2500)       : 500 threads 2000 times 0.428s
0484.0 ns/inv Boolean(1500)       : 500 threads 2000 times 0.484s
 */