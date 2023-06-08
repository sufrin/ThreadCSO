package ox.net.codec


/**
  * Tests of the `Stream[T]`-encoding specification DSL.
  *
  * The implicit encodings specified below are named conventionally
  * after the types they encode. This is not mandatory, but
  * it helps to construct the encodings systematically, and
  * to see whether an existing corpus of encodings is complete.
  *
  */
object StreamEncodingShortTests {

  import DataStreamEncoding._

  object Day extends Enumeration {
    val Mon, Tue, Wed = Value
  }

  type Day = Day.Value

  implicit object `Day*` extends `Enum*`[Day](Day.apply, _.id)

  implicit object `Seq[Day]*` extends `Seq*`[Day]

  implicit object `Set[Day]*` extends `Set*`[Day]

  implicit object `Option[Day]*` extends `Option*`[Day]

  /////////////////////////////////////////////////////////////////////////

  case class Record(name: String, value: Int)

  implicit object `Record*` extends `2-Case*`[Record, String, Int](Record.apply, Record.unapply)

  /////////////////////////////////////////////////////////////////////////


  /**
    * Technique for recursive sums of products: note that
    * `B3` is recursive in `K` so it needs the
    * `2-Case-Rec*`-implicit encoding, which itself
    * needs explicitly named component encodings
    */
  trait K

  case object B  extends K
  case object BB extends K
  case class  B1(n: Int, s: String) extends K
  case class  B2(n: Int, s: String) extends K
  case class  B3(k1: K, k2: K) extends K


  implicit object `B*`   extends `Case*`(B)
  implicit object `BB*`  extends `Case*`(BB)
  implicit object `B1*`  extends `2-Case*`[B1, Int, String](B1.apply, B1.unapply)
  implicit object `B2*`  extends `2-Case*`[B2, Int, String](B2.apply, B2.unapply)
  implicit object `B3*`  extends `2-Case-Rec*`[B3, K, K](B3.apply, B3.unapply)(`K*`, `K*`)

  implicit object `K*`   extends
           `5-Union*`[K,  B1, B2, B3, BB.type, B.type](
             _.asInstanceOf[B1], _.asInstanceOf[B2], _.asInstanceOf[B3], _.asInstanceOf[BB.type], _.asInstanceOf[B.type]
           ) //(`B1*`, `B2*`, `B3*`, `BB*`, `B*`)


  implicit object `Seq[K]*` extends `Seq*`[K]

  /////////////////////////////////////////////////////////////////////////

  implicit object `(String,Int)*` extends `2-Tuple*`[String, Int]

  implicit object `Seq[Int]*` extends `Seq*`[Int]

  implicit object `Seq[(String,Int)]*` extends `Seq*`[(String, Int)]

  implicit object `Seq[String]*` extends `Seq*`[String]

  implicit object `List[String]*` extends `List*`[String]

  implicit object `Array[String]*` extends `Array*`[String]

  implicit object `Seq[Record]*` extends `Seq*`[Record]

  implicit object `(Int,Int,Record,Double,Seq[String],Seq[Int])*` extends `6-Tuple*`[Int, Int, Record, Double, Seq[String], Seq[Int]]

  implicit object `(Int,Int,Record,Double,List[String])*` extends `5-Tuple*`[Int, Int, Record, Double, List[String]]

  implicit object `(Option[Day],Int,Record,Double,Array[String])*` extends `5-Tuple*`[Option[Day], Int, Record, Double, Array[String]]

  /////////////////////////////////////////////////////////////////////////

  implicit object `(Int,Int,Record,Double,Array[String])*`
    extends `5-Tuple*`[Int, Int, Record, Double, Array[String]]

  implicit object `(Int,Int,Record,Double,Seq[String])*`
    extends `5-Tuple*`[Int, Int, Record, Double, Seq[String]]

  implicit object `(Option[Day],Int,Record,Double,List[String])*`
    extends `5-Tuple*`[Option[Day], Int, Record, Double, List[String]]

  implicit object `(Option[Day],Int,Record,Double,Seq[String])*)`
    extends `5-Tuple*`[Option[Day], Int, Record, Double, Seq[String]]

  /////////////////////////////////////////////////////////////////////////

  // Serialization of an arbitrary structure isn't expected to pass the exact roundtrip test

  class Foo(s: String) extends Serializable {
    var n: Int = 0
    val bars = new Node(new Bar(s), new Node(new Bar(s), null))
    // make it cyclic
    bars.next.next = bars

    //
    override def toString: String = {
      n = n + 1
      s"Foo($s) $n $bars"
    }
  }

  case class Node[T](s: T, var next: Node[T]) extends Serializable {

    private def toList(node: Node[T]): (Boolean, List[Node[T]])  = {
      val visited = new collection.mutable.ListBuffer[Node[T]]()
      var current = node
      while ((current ne null) && !visited.exists(_.eq(current))) {
          visited.append(current)
          current = current.next
      }
      (current ne null, visited.toList)
    }

    override def toString: String = {
      val (cyclic, nodes) = this.toList(this)
      nodes.map(_.s).mkString("(", "->", if (cyclic) "->...)" else ")")
    }

    override def equals(that: Any): Boolean = that match {
      case that: Node[T] @unchecked => this.toString == that.toString
      case _ => false
    }
  }

  class Bar(s: String) extends Serializable {
    override def toString: String = s"Bar($s)"
  }

  implicit object `Foo*` extends `Serializable*`[Foo]

  /////////////////////////////////////////////////////////////////////////

  def main(args: Array[String]): Unit = {
    import DataStreamEncoding.{encodingTest => test}
    test(1)
    test("two")
    test(("three", 4))
    test((1, 2, Record("foo", 42), 3.2, Seq("A String", "Another String")))
    // Ad-hoc type annotations for underdetermined types
    test(
      (
        Some(Day.Mon): Option[Day],
        2,
        Record("foo", 42),
        3.2,
        Seq("A String", "Another String")
      )
    )
    test(
      (
        None: Option[Day],
        2,
        Record("foo", 42),
        3.2,
        List("A String", "Another String")
      )
    )
    type AdHoc = (Option[Day], Int, Record, Double, List[String])
    test[AdHoc](
      (None, 2, Record("foo", 42), 3.2, List("A String", "Another String"))
    )
    test(Record("foo", 34))
    test(Array("What do you think", "Mr Apocalypse"))
    test(Seq(Day.Mon, Day.Tue, Day.Wed))
    test(Set(Day.Mon, Day.Tue, Day.Wed, Day.Mon, Day.Tue))
    test(B2(3, "b2"))
    test(B1(3, "b1"))
    test(B3(B1(3, "b1"), BB))
    test(B3(B1(3, "b1"), B))
    test(B3(B1(3, "b1"), B3(B1(3, "b1"), BB)))
    test(Array("What do you think", "Mr Apocalypse"))
    // A limitation of union encoding: the inferred type of the literal 2-sequence is
    // Seq[Product with K with java.io.Serializable]
    test[Seq[K]](Seq(B2(2, "b2"), B1(1, "b1")))
    test[Seq[K]](Seq(B2(2, "b2")))
    test[Seq[K]](Seq(BB, B2(2, "b2"), B))
    // -------------

    /**
      * This encoding  works only for `acyclic` `Node` structures.
      * The recursive+union construction of the specification is essential for dealing with
      * `null`-terminated chains of `Node`; as is the explicit provision of the implicit parameter
      * for test. Here `Node?[Int]*` is the stream encoding for a possibly-null `Node[Int]`.
      */
    val b: Node[Int] = Node(1, Node(2, null))
    implicit object `?Node[Int]*` extends `2-Union*`[Node[Int], Null, Node[Int]](_.asInstanceOf[Null], _.asInstanceOf[Node[Int]])(`Null*`, `Node[Int]*`)
    implicit object `Node[Int]*`  extends  `2-Case-Rec*`[Node[Int], Int, Node[Int]](Node.apply, Node.unapply)(`Int*`, `?Node[Int]*`)
    test(b)(`Node[Int]*`)

    // ----------------
    try {
      /** This specification will fail because of the `null` at the end of the list */
      lazy val `Node[Int]**`: Stream[Node[Int]] = new`2-Case-Rec*`[Node[Int], Int, Node[Int]](Node.apply, Node.unapply)(`Int*`, `Node[Int]**`)
      test(b)(`Node[Int]**`)
    } catch {
      case exn: Throwable => println(s"**** Encoding failure as expected:\n**** Node.unapply(null) fails at the end of the chain:\n**** ${exn.toString}")
    }
    // ----------------
    try {
      /** This specification succeeds, because `Serializable*` can encode object graphs */
      object `Node[Int]**` extends `Serializable*`[Node[Int]]
      b.next.next = Node(999, b)
      test(b)(`Node[Int]**`)
    } catch {
      case exn: Throwable =>
           println("Exception $exn during roundtrip")
    }

    // ----------------
    println("****\n**** Expected roundtrip failure(s)\n**** (Types without natural structural equality fail)\n****")
    test((1, 2, Record("Expected", 42), 3.2, Array("What do you think", "Mr Apocalypse")))
    val f = new Foo("aFoo")
    test(f)


  }
}
