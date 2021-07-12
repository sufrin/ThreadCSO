import scala.collection.mutable.Iterable
import Graph.Vertex

/**
  * An immutable view of a graph
  *
  * @see [[AdjacencyGraph]]
  */
trait Graph extends Iterable [(Vertex, Iterable[Vertex])]
{
  /** Iterator over the vertices */
  def vertices: Iterator[Vertex]
  /** Iterator over `(vertex, adjacency-set-iterable)` pairs */
  def iterator: Iterator[(Vertex, Iterable[Vertex])]
  /** The set of vertices adjacent to the given `vertex` */
  def adjacent(edge: Vertex): Iterable[Vertex]
  /** The number of edges */
  def edgeCount: Int
  /** The number of vertices */
  def vertexCount: Int
  /** Iterator over the edges */
  def edges: Iterator[(Vertex, Vertex)]

  /** String representing the graph in adjacency-list form */
  override def toString: String =
  { val s = new StringBuilder()
    for ((v, as) <- iterator)
    { s ++= v
      s ++= "\n"
      for (a <- as) { s ++= "  "; s ++= a; s ++= "\n" }
    }
    s.toString
  }
}

object Graph
{
  type Vertex = String

  /** An empty graph */
  val empty: Graph               = AdjacencyGraph.empty

  /** Construct the adjacency-set representation of the graph that is described
    * by its edges in the file denoted by `path`.
    *
    * Each edge is represented on a single line, in the form: `startVertex -> endVertex`
    */
  def apply(path: String): Graph = AdjacencyGraph.apply(path)

  /**
    * Trim leading and trailing spaces and trailing `'/'`
    */
  val trimURL: String => String = AdjacencyGraph.normalize _
}
