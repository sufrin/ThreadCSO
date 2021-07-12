import Graph.Vertex
import scala.collection.mutable.{Iterable, Set, Map, TreeMap}

/**
  * A factory for graphs represented in adjacency set form.
  */

object AdjacencyGraph
{ type Vertex = String

  /** A wrapper class for the direct representation as a mapping */

  protected class AdjGraph(protected val graph: Map[Vertex, Set[Vertex]]) extends Graph
  {
    /** Iterator over the vertices */
    def vertices: Iterator[Vertex] = graph.keysIterator

    /** Iterator over `(vertex, adjacency-set)` pairs */
    def iterator: Iterator[(Vertex, Iterable[Vertex])] = graph.iterator

    /** Iterator over the set of vertices adjacent to the given `vertex` */
    def adjacent(edge: Vertex): Iterable[Vertex] = graph.getOrElse(edge, Set.empty[Vertex])

    /** The number of edges */
    def edgeCount: Int = {
      var n = 0
      for ((_, a)<-graph) n+=a.size
      n
    }

    /** The number of vertices */
    def vertexCount: Int = graph.size

    /** Iterator over the edges */
    def edges: Iterator[(Vertex, Vertex)] =
    { val vertices = graph.iterator
      vertices.flatMap { case (v, as) => for (a<-as) yield (v, a) }
    }

  }


  /** An empty graph */
  val empty = new AdjGraph(mkAdjGraph())

  /**
    * The graph described in the file denoted by `path` in the form of
    * a sequence of lines, each containing an edge description of the form
    * `startVertex -> endVertex`
    *
    */
  def apply(path: String): Graph = new AdjGraph(mkAdjGraph(path))

  /** Remove leading spaces, as well as trailing spaces and `/` */
  def normalize(s: Vertex): Vertex =
  { val trimmed = s.trim
    if (trimmed.endsWith("/")) trimmed.substring(0, trimmed.size-1) else trimmed
  }

  /** Iterate over the edge descriptions in the file at `path` */
  def forEdges(path: String)(doEdge: (Vertex, Vertex) => Unit) =
   {
     import java.io.File
     import scala.io.Source

     val arrow     = " -> "
     val arrowSize = arrow.size
     try
     {
       for (line <- Source.fromFile(new File(path), "UTF-8").getLines())
         { val pos = line.indexOf(arrow)
           if (pos>=0)
           { val source = normalize(line.substring(0, pos))
             val dest   = normalize(line.substring(pos + arrowSize))
             doEdge(source, dest)
           }
         }
     }
     catch
     {
       case exn: java.io.FileNotFoundException => Console.err.println(s"File not found: $path")
     }
   }

   import collection.mutable._

   /**
     * Build the adjacency-set representation of the directed graph described in the file at `path`. If
     * `path` isn't supplied, then build a representation of the empty graph.
     */
   def mkAdjGraph(path: String = null): Map[Vertex, Set[Vertex]] =
   {
     val edgeMap = new TreeMap[Vertex, Set[Vertex]]

     def getAdjacent(s: Vertex) =
     {
       edgeMap.get(s) match
         { case None    => val t = new TreeSet[Vertex]; edgeMap.put(s, t); t
           case Some(t) => t
         }
     }

     def addEdge(s: Vertex, d: Vertex): Unit =
     {
       val edges = getAdjacent(s)
       val _     = getAdjacent(d)  // for completeness of the domain of `edgeMap`
       edges.add(d)
     }

     if (path!=null) forEdges(path)(addEdge)
     edgeMap
   }

   def main(args: Array[Vertex]): Unit =
   {
     for (arg<-args)
     {
       val graph = Graph(arg)
       println(graph.toString)
     }
   }
}