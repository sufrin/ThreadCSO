import io.threadcso._

/** <p> Simple trial of the barrier implementation.
  *
  * Expected output from a no-argument trial
  * {{{
  * 0	    0     1     2     3     4 	    0     3     5     7     4
  * 1	    0     3     5     7     4 	    0     8    12    11     4
  * 2	    0     8    12    11     4 	    0    20    23    15     4
  * 3	    0    20    23    15     4 	    0    43    38    19     4
  * 4	    0    43    38    19     4 	    0    81    57    23     4
  * 5	    0    81    57    23     4 	    0   138    80    27     4
  * 6	    0   138    80    27     4 	    0   218   107    31     4
  * 7	    0   218   107    31     4 	    0   325   138    35     4
  * 8	    0   325   138    35     4 	    0   463   173    39     4
  * 9	    0   463   173    39     4 	    0   636   212    43     4
  * 10	   0   636   212    43     4 	    0   848   255    47     4
  * 11	   0   848   255    47     4 	    0  1103   302    51     4
  * 12	   0  1103   302    51     4 	    0  1405   353    55     4
  * 13    0  1405   353    55     4 	    0  1758   408    59     4
  * 14    0  1758   408    59     4 	    0  2166   467    63     4
  * 15	   0  2166   467    63     4 	    0  2633   530    67     4
  * 16	   0  2633   530    67     4 	    0  3163   597    71     4
  * 17	   0  3163   597    71     4 	    0  3760   668    75     4
  * 18	   0  3760   668    75     4 	    0  4428   743    79     4
  * 19	   0  4428   743    79     4 	    0  5171   822    83     4
  * }}}
  *
  * Variations can be played on omitting sync calls, using a single sync in two
  * phases, etc. etc.
  */

object BarrierTrial {
  import io.threadcso.debug.Logger
  val log = Logger("Barrier", 200)
  def main(args: Array[String]): Unit = {
    var N = 5
    var stallC, stallU = -1
    var sep = true
    var deb = false
    for (arg <- args)
      if (arg.matches("[0-9]+")) N = arg.toInt
      else if (arg.matches("-u=[0-9]+")) {
        deb = true; stallU = arg.substring(3).toInt
      } else if (arg.matches("-c=[0-9]+")) {
        deb = true; stallC = arg.substring(3).toInt
      } else if (arg.matches("-s")) sep = false
      else if (arg.matches("-d")) deb = true
      else {
        println(
          """Usage: -d start debugger   | 
             <int> -- radius of problem | 
             -s -- use a single barrier | 
             -u=<n> -- forget update sync at round <n> | 
             -c=<n> forget compute sync at round <n>"""
        )
        exit()
      }
    if (deb) println(debugger)
    var t, u = Array.ofDim[Int](N)
    val cb = Barrier(N, "Calculate")
    val ub = if (sep) Barrier(N, "Update") else cb
    for (i <- 0 until N) { t(i) = i; u(i) = 0 }
    def printt(t: Seq[Int]) = if (N<10) for (i <- 0 until N) print(f"${t(i)}%5s ")
    def agent(n: Int) =
      proc(s"agent($n)") {
        var i = 0
        var time = 0L
        while (i < 20) {
          if (!(n == 0 && i == stallC)) {
             val now = nanoTime
             cb.sync()
             if (n==0) time += (nanoTime - now)
          }
          log.log(-1, s"$n calculates")
          if (n != 0) {
            u(n) = t(n) + t((n + 1) % N)
            sleep((math.random() * 250).toLong * milliSec)
          }
          if (!(n == 0 && i == stallU)) ub.sync()
          log.log(-1, s"$n updates")
          if (n == 0) {
            print(f"$i%5d"); printt(t.toIndexedSeq); print("\t"); printt(u.toIndexedSeq);
            print(f"${time.toDouble/N}%8gns sync")
            println()
            val tt = t; t = u; u = tt
          }
          i += 1
        }
      }
    run(||(for (i <- 0 until N) yield agent(i)))
    if (!deb) exit()
  }
}
