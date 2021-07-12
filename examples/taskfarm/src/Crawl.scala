import io.threadcso._
import Soup.links

/** This is a sketch of a bag-of-tasks style web crawler */

object Crawl
{  type Job = String
   type Arc = (Job, Job)

   def controller(root: Job, jobsIn: ?[Option[Arc]], jobsOut: ![Job]): PROC = proc("controller")
   {  val jobs     = new collection.mutable.HashSet[Job]
      val visited  = new collection.mutable.HashSet[Job]
      @inline def dequeueJob(): Job = { val job = jobs.head; jobs.remove(job); job}
      @inline def enqueueJob(job: Job) = jobs.add(job)
      @inline def isQueued(job: String) = jobs.contains(job)
      var busyWorkers = 0
      enqueueJob(root)
      try
      {
        serve(
                 ((jobs.nonEmpty) && jobsOut) =!=>
                 {
                   busyWorkers += 1
                   val job = dequeueJob()
                   println(job)
                   visited.add(job)
                   job
                 }

               | (busyWorkers > 0 && jobsIn) =?=>
                 { case Some((source, job)) =>
                        println(s"$source -> $job")
                        if (!visited.contains(job) && !isQueued(job)) enqueueJob(job)
                   case None      => busyWorkers -= 1
                 }
             )
        println("-------------- CONTROLLER TERMINATED --------------")
      } catch {
        case exn: Exception => exn.printStackTrace()
      }
     jobsOut.closeOut
     jobsIn.closeIn
     for (v<-visited) println(v)
   }

  def worker(me: Int, site: String, jobsIn: ?[Job], jobsOut: ![Option[Arc]]): PROC = proc(s"worker($me)")
  {  repeat {
       val url = jobsIn?()
       sleep(seconds(0.15)) // don't hammer the poor server
          if (url.endsWith(".html") || !url.matches("""^.*\.[a-zA-Z0-9]+"""))
             for (link <- links(url, site)) jobsOut ! Some((url, link))
       jobsOut!None
     }
     // println(s"-------- Worker($me) TERMINATED ------------")
     jobsIn.closeIn
     jobsOut.closeOut
  }

  def main(args: Array[String]): Unit =
  {
    var N    = 5
    var site = ""
    for (arg<-args)
      if (arg=="-D")  println(debugger) else
      if (arg.startsWith("-s=")) site = arg.substring(3) else
      if (arg.matches("-N=[0-9]+")) N = arg.substring(3).toInt else
      { val toWorkers     = N2N[Job](writers=1, readers=N, name=s"toWorkers")
        val fromWorkers   = N2NBuf[Option[Arc]](5000, writers=N, readers=1, name="fromWorkers")
        val theWorkers    = || (for (i<-0 until N) yield worker(i, site, toWorkers, fromWorkers))
        val theController = controller(arg, fromWorkers, toWorkers)
        (theController || theWorkers)()
      }
  }

}
