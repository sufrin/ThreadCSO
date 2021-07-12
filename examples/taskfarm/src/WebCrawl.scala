import io.threadcso._
import Graph._
import collection.mutable.{Set, HashSet,TreeSet}

/**
  * Simulation of a web crawler using a preloaded graph of URLs to simulate the Web
  *
  * One-one channels between controller and workers: serve loop has N+1 branches
  *
  * @see [[WebCrawler]]
  *
  */


object WebCrawl
{
  var cMonitor, dMonitor, eMonitor, gMonitor, pMonitor, wMonitor = false

  type  URL = String
  type Link = (URL, URL)

   def controller(root: URL, jobsIn: ?[Option[Link]], jobsOut: Seq[![URL]], visited: Set[URL]): PROC = proc("controller")
   {  val jobs   = new collection.mutable.Queue[URL]
      val unseen = new HashSet[URL]
      //
      // we decided, somewhat arbitrarily, to explore breadth-first.
      // Using unseen as a pre-filter for putting things in the queue makes this a tad more efficient,
      //
      // INVARIANT: unseen == jobs.asSet
      //
      var busyWorkers = 0
      var urlCount, receivedCount, duplicated = 0
      jobs.enqueue(root); unseen.add(root)
      try
      {
        serve(
               |(for (out <- jobsOut) yield
                 (jobs.nonEmpty && out) =!=>
                 {
                   busyWorkers += 1
                   val job = jobs.dequeue()
                   unseen -= job
                   urlCount += 1
                   visited.add(job)
                   if (pMonitor && urlCount % 100==0) Console.err.print(s"$urlCount\r")
                   job
                 }
                )
               | (busyWorkers > 0 && jobsIn) =?=>
                 { case Some((source, job)) =>
                        if (eMonitor) println(s"$source -> $job")
                        if (!visited.contains(job)) {
                           receivedCount+= 1
                           if (unseen.contains(job)) duplicated += 1 else { jobs.enqueue(job) ; unseen.add(job) }
                        }
                   case None => busyWorkers -= 1
                 }
             )
      } catch {
        case exn: Exception => exn.printStackTrace()
      }
     for (out <- jobsOut) out.closeOut
     jobsIn.closeIn
     if (cMonitor) Console.err.println(s"Controller: Dispatched $urlCount out of $receivedCount received ($duplicated duplicated)")
   }

  def worker(me: Int, web: Graph, jobsIn: ?[URL], jobsOut: ![Option[Link]]): PROC = proc(s"worker($me)")
  {  var urlCount = 0
     repeat
     {
       val url = jobsIn?()
       urlCount += 1
       sleep(connectTime)
       for (link <- web.adjacent(url)) { sleep(linkTime); jobsOut ! Some((url, link)) }
       jobsOut!None
     }
     jobsIn.closeIn
     jobsOut.closeOut
     if (wMonitor) Console.err.println(s"worker($me) $urlCount")
  }

  var simulatedDelay  = 0.1
  def connectTime     = seconds(simulatedDelay)
  def linkTime        = seconds(simulatedDelay/10.0)

  def crawler(web: Graph, start: String, workers: Int, list: Boolean)
  { val visited    = new HashSet[String] // O(1) performance is important during the crawl
  val url         = trimURL(start)

    val toWorkers   = for (i<-0 until workers) yield OneOne[URL](name=s"toWorker($i)")
    val fromWorkers = N2NBuf[Option[Link]](5000, writers=workers, readers=1, name="fromWorkers")

    val theWorkers    = || (for (i<-0 until workers) yield worker(i, web, toWorkers(i), fromWorkers))
    val theController = controller(url, fromWorkers, toWorkers, visited)

    val startTime = nanoTime
    // measure only the crawl time
    (theController || theWorkers)()
    val runTime = nanoTime-startTime
    println(s"-N=$workers $simulatedDelay size: ${visited.size}/${web.vertexCount}/${web.edgeCount} ${runTime.hms} ")

    // Listing in alphabetical order
    if (list) {
      val sorted = new TreeSet[String]
      for (site<-visited) sorted.add(site)
      for (site<-sorted)  println(site)
    }
  }


  def main(args: Array[String]): Unit =
  {
    /** We simulate the web with an adjacency-set graph representing urls connected by links */
    var graph = Graph.empty
    var N               = 5
    var list            = false
    var lastSearch      = ""

    for (arg<-args)
      if (arg.matches("-[D]"))            println(debugger)else
      if (arg.matches("-[d]"))            dMonitor  = true else
      if (arg.endsWith(".edges"))         graph     = Graph(arg)else
      if (arg.matches("-[Nn]=[0-9]+"))    N         = arg.substring(3).toInt else
      if (arg.matches("-[Ll]"))           list      = true else
      if (arg.matches("-[Cc]"))           cMonitor = true else
      if (arg.matches("-[Ww]"))           wMonitor = true else
      if (arg.matches("-[Gg]"))           gMonitor = true else
      if (arg.matches("-[Pp]"))           pMonitor = true else
      if (arg.matches("-[Ee]"))           eMonitor = true else
      if (arg.matches("[0-9]*\\.[0-9]+")) simulatedDelay = arg.toDouble else
      if (arg.matches("[0-9]+")) for (i<-0 until arg.toInt) crawler(graph, lastSearch, N, list) else
      {
        lastSearch = arg
        crawler(graph, lastSearch, N, list)
      }
    exit()
  }

}
