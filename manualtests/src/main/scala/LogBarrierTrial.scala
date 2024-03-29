import io.threadcso._
import io.threadcso.lock.LogBarrier

/** Simple trial of the barrier implementation.
  *
  * Expected output from a no-argument trial
  * {{{
  * 0       0     1     2     3     4       0     3     5     7     4
  * 1       0     3     5     7     4       0     8    12    11     4
  * 2       0     8    12    11     4       0    20    23    15     4
  * 3       0    20    23    15     4       0    43    38    19     4
  * 4       0    43    38    19     4       0    81    57    23     4
  * 5       0    81    57    23     4       0   138    80    27     4
  * 6       0   138    80    27     4       0   218   107    31     4
  * 7       0   218   107    31     4       0   325   138    35     4
  * 8       0   325   138    35     4       0   463   173    39     4
  * 9       0   463   173    39     4       0   636   212    43     4
  * 10      0   636   212    43     4       0   848   255    47     4
  * 11      0   848   255    47     4       0  1103   302    51     4
  * 12      0  1103   302    51     4       0  1405   353    55     4
  * 13      0  1405   353    55     4       0  1758   408    59     4
  * 14      0  1758   408    59     4       0  2166   467    63     4
  * 15      0  2166   467    63     4       0  2633   530    67     4
  * 16      0  2633   530    67     4       0  3163   597    71     4
  * 17      0  3163   597    71     4       0  3760   668    75     4
  * 18      0  3760   668    75     4       0  4428   743    79     4
  * 19      0  4428   743    79     4       0  5171   822    83     4
  * }}}
  *
  * Variations can be played on omitting sync calls, using a single sync in two
  * phases, etc. etc.
  */
object LogBarrierTrial 
{
  import io.threadcso.debug.Logger
  val log = Logger("LogBarrierTrial", 200)
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
             <int> -- size of problem |
             -s -- use a single barrier | 
             -u=<n> -- forget update sync at round <n> | 
             -c=<n> forget compute sync at round <n>"""
        )
        exit()
      }
    if (deb) println(debugger)
    var t, u = Array.ofDim[Int](N)
    val cb = new LogBarrier(N, "Calculate")
    val ub = if (sep) new LogBarrier(N, "Update") else cb
    for (i <- 0 until N) { t(i) = i; u(i) = 0 }

    def printt(t: Seq[Int]) =
        if (N<10) for (i <- 0 until N) print(f"${t(i)}%5s ")

    def agent(n: Int) =
      proc(s"agent($n)") {
        var time = 0L
        var i = 0
        while (i < 20) {
          var time = 0L
          if (!(n == 0 && i == stallC)) {
             val now = nanoTime
             cb.sync(n)
             if (n==0) time += (nanoTime - now)
          }
          log.log(-1, s"$n calculates")
          if (n != 0) {
            u(n) = t(n) + t((n + 1) % N)
            sleep((math.random() * 250).toLong * milliSec)
          }
          if (!(n == 0 && i == stallU)) ub.sync(n)
          log.log(-1, s"$n updates")
          if (n == 0) {
            print(f"${i}%5d");
            printt(t.toIndexedSeq); print("\t"); printt(u.toIndexedSeq);
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

/** Simple trial of the combininglogbarrier
  *
  * The program runs N agents, each of whom decrements an internal
  * counter (initialized to its identity) until the consensus is
  * that the sum of the counters is nonpositive. Agent 0 prints the
  * consensus sum at each round.
  *
  * A stall can be provoked by agent N/2 on the round given by `-s=theRound`,
  * and in this case debugger is automatically started as the trial begins.
  *
  * The following outputs are as expected for a 50-agent (the default count) system
  *
  * ./runexample CombiningLogBarrierTrial -s=20
  * Debugger(http://localhost:9999)
  * 1:1225 2:1175 3:1125 4:1075 5:1025 6:975 7:925 8:875 9:825 10:775
  * 11:725 12:675 13:625 14:575 15:525 16:475 17:425 18:375 19:325 20:275
  * 25 stalling
  * ^C
  *
  * 1981 % ./runexample CombiningLogBarrierTrial
  * 1:1225 2:1175 3:1125 4:1075 5:1025 6:975 7:925 8:875 9:825 10:775
  * 11:725 12:675 13:625 14:575 15:525 16:475 17:425 18:375 19:325 20:275
  * 21:225 22:175 23:125 24:75 25:25 26:-25
  *
  */
object CombiningLogBarrierTrial
{ import io.threadcso.debug.Logger
  val log = Logger("LogBarrier", 200)
  def main(args: Array[String]): Unit = {
    var N = 50
    var stall = -1
    var deb = false
    for (arg <- args)
      if (arg.matches("[0-9]+")) N = arg.toInt
      else if (arg.matches("-s=[0-9]+")) {
        deb = true; stall = arg.substring(3).toInt
      }
      else if (arg.matches("-d")) deb = true
      else {
        println(
          """Usage: -d start debugger   |
             <int> -- (default $N) Number of agents |
             -s=<n> -- agent 0 forgets a sync at round <n> """
        )
        exit()
      }
    if (deb) println(debugger)
    val cb = CombiningLogBarrier(N, 0, { (m: Int, n: Int) => m+n }, null)

    def agent(me: Int) =
      proc(s"agent($me)") {
        var local = me
        var round = 0
        var going = true
        while (going) {
          sleep(seconds(0.5*scala.util.Random.nextDouble()))
          var tot = 0
          if (round==stall && me==N/2) {
              println(s"($me stalling)")
          }
          else
            tot = cb.sync(me)(local)
          round += 1
          if (me==0) print(s" $round:$tot")
          if (me==0&&round%10==0) println()
          going = tot>0
          local -= 1
        }
        if (me==0) println("")
      }

    run(||(for (i <- 0 until N) yield agent(i)))
    if (!deb) exit()
  }
}

