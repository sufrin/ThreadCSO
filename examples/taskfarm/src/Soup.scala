import org.jsoup._
import org.jsoup.nodes.{Document, Element}

/**
  *  A module with methods that support the retrieval of links from
  *  html documents denoted by urls using one of the protocol prefixes
  *  {{{
  *    http://
  *    https://
  *    file://
  *  }}}
  *
  *  or (without protocol prefix) ordinary files
  *
  *  Documents whose urls have `http` prefixes are fetched from the server specified in the url;
  *  others are read locally.
  */
object Soup
{
  /** Does the given `url` start with `http://` or `https://` */
  def isAbsolute(url: String) = url.startsWith("http://") || url.startsWith("https://")

  /** Is the given `url`  unacceptable because it has a non-http protocol, or because
    * it is a reference to an anchor, or because it takes the form of a query.
    */
  def isExcluded(url: String) =
          url.contains("?") || url.contains("#") ||  (url.contains(":") && !isAbsolute(url))

  /** Catenate the two paths `l`, `r` without unnecessarily duplicating "/" at the join. */
  def catPath(l: String, r: String): String =
  { val ll = l.endsWith("/")
    val rr = r.startsWith("/")
    (ll, rr) match {
      case (true, true)   => l++r.substring(1)
      case (true, false)  => l++r
      case (false, true)  => l++r
      case _ => l++"/"++r
    }
  }

  def canonicalize(path: String) = new java.io.File(path).getCanonicalPath

  /** Read an html document from the location specified by the given `url`. If the
    * `url` is not absolute, then it is taken to be a file in the local filestore.
    *
    * @param timeoutMS -- how long to wait for the document
    */
  def readDocument(url: String, timeoutMS: Int = 5000): Document =
     if (isAbsolute(url))
        Jsoup.connect(url).timeout(timeoutMS).get
     else
     { val path = if (url.startsWith("file://")) url.substring(7) else url
       val in   = new java.io.File(path)
       Jsoup.parse(in, "UTF-8", in.getCanonicalFile.getParent)
     }

  def text(url: String) =
    try { readDocument(url).body.text() }
    catch { case exn: java.io.IOException => exn.printStackTrace(Console.err); "" }

  /**
    * Return the sequence of links (by links) from the document denoted by the given `url`. References
    * are only returned if they are prefixed by `onSite` (which should be the empty string if any reference
    * is acceptable).  References appear in the sequence no more than once.
    */
  def links(url: String, onSite: String): Seq[String] =
  {
    try
    {
      references(readDocument(url), onSite)
    }
    catch
      {
        case exn: Exception => println(s"$exn reading $url"); List.empty
      }
  }

   /**
    * Return the sequence of links (by links) from the given `Document`. References
    * are only returned if they are prefixed by onSite (which should be the empty string if any reference
    * is acceptable). References appear in the sequence no more than once.
    */
  def references(d: Document, onSite: String): Seq[String] =
  { val base    = d.baseUri()
    val result  = new scala.collection.mutable.Queue[String]
    val anchors = d.select("a[href]")
    val it = anchors.iterator()
    while (it.hasNext)
      { val elt    = it.next()
        val absRef = elt.attr("abs:href")
        val link   = elt.attr("href")
        val ref    = if (absRef!="") absRef else if (link!="") canonicalize(catPath(base, link)) else ""
        if (ref!="" && ref.startsWith(onSite) && !isExcluded(ref) && !result.contains(ref)) result.enqueue(ref)
      }
    result.toSeq
  }

  /**
    * Soup is itself runnable. It outputs the permissible links (from each argument url or file) that
    * are prefixed with the string '''sitePrefix'''. This prefix is initially empty, and can set by giving
    * the argument -s=`'''sitePrefix''''.
    */
  def main(args: Array[String]): Unit =
  { var onSite = ""
    for (arg<-args)
      if (arg.startsWith("-s="))
        onSite = arg.substring(3)
      else
      for (ref<- links(arg, onSite))
        {
          println(ref)
        }
  }
}

