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
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
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

  /*
   * Notice the explicit provision of `K*` for the encodings of recursive instances of `K` in `B3`
   * Because `K*` is a forward reference (one of `B3*`, `K*` has to be), the encoding parameters
   * of `B3` have to be thunks.
   */
  implicit object `B3*`  extends `2-Case-Rec*`[B3, K, K](B3.apply, B3.unapply)(`K*`, `K*`)

  implicit object `K*`   extends
    `5-Union*`[K,  B1, B2, B3, BB.type, B.type](_.asInstanceOf[B1], _.asInstanceOf[B2], _.asInstanceOf[B3], _.asInstanceOf[BB.type], _.asInstanceOf[B.type])


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

  /**
    * Serialization of an structure by `Serializable*` passes the
    * roundtrip test if equality is defined properly.
    */


  class Foo(s: String) extends Serializable {
    var creation: Long = io.threadcso.milliTime
    val bars = new Node(new Bar(s), new Node(new Bar(s), null))
    // make it cyclic
    bars.next.next = bars

    //
    override def toString: String = {
      s"Foo($s)@$creation $bars"
    }

    override def equals(that: Any): Boolean = that match {
      case that: Foo => toString == that.toString
      case _ => false
    }
  }

  implicit object `Foo*` extends `Serializable*`[Foo]

  /**
    * Representation of a potentially cyclic singly-linked chain of nodes.
    * Provides test-cases for the encoding/decoding of both cyclic
    * and acyclic structures.
    */
  case class Node[T](datum: T, var next: Node[T]) extends Serializable {

    /**
      *  Return the path (as a list of nodes) from `node` to the end
      *  of the chain (if it ends in `null`) or to the first back-pointer
      *  encountered to the path (if the chain is cyclic).
      */
    private def toList(node: Node[T]): (Boolean, List[Node[T]])  = {
      val visited = new collection.mutable.ListBuffer[Node[T]]()
      var current = node
      while ((current ne null) && !visited.exists(_.eq(current))) {
          visited.append(current)
          current = current.next
      }
      (current ne null, visited.toList)
    }

    /**
      * Print the data in the chain starting at `this` and ending at `null`
      * or the first back pointer into the chain. Distinguish between a
      * cyclic and an acyclic chain.
      */
    override def toString: String = {
      val (cyclic, nodes) = this.toList(this)
      nodes.map(_.datum).mkString("(", "->", if (cyclic) "->...)" else ")")
    }

    /**
      * The data in the chain reachable from `this` is the same as the data in
      * the chain reachable from `that`, if `that` is a `Node`.
      */
    override def equals(that: Any): Boolean = that match {
      case that: Node[T] @unchecked => this.toString == that.toString
      case _ => false
    }
  }

  class Bar(s: String) extends Serializable {
    override def toString: String = s"Bar($s)"
  }


  /////////////////////////////////////////////////////////////////////////

  def main(args: Array[String]): Unit = {
    import DataStreamEncoding.{encodingTest => test}
    test(1)
    test("two")
    test(("three", 4))
    test((1, 2, Record("foo", 42), 3.2, Seq("A String", "Another String")))

    println("------------  Map --------------")
    implicit object `Map[Int,String]*` extends `PairsMap*`[Int, String]
    type MyMap = Map[Int, String]

    val m = PairMap(1->"one", 2->"two")
    test(m)

    val mm = PairMap(1 -> List(1,2), 2 -> List(3,4))
    implicit object `List[Int]*` extends `List*`[Int]
    implicit object `Map[Int,List[Int]]*` extends  `Map*`[Int,List[Int]]
    test(mm)

    val nextDay = {
      import Day._; PairMap(Mon -> Tue, Tue -> Wed, Wed -> Thu)
    }
    implicit object `Map[Day,Day]*` extends `PairsMap*`[Day, Day]
    test(nextDay)

    println("------------  Option --------------")

    // Ad-hoc type annotations are needed for underdetermined types
    test((Some(Day.Mon): Option[Day], 2, Record("foo", 42), 3.2, Seq("A String", "Another String")))
    test((None: Option[Day], 2, Record("foo", 42), 3.2, List("A String", "Another String")))
    type AdHoc = (Option[Day], Int, Record, Double, List[String])
    test[AdHoc]((None, 2, Record("foo", 42), 3.2, List("A String", "Another String")))

    println("------------  Assortment --------------")

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

    println("------------  Inductive --------------")

    // A limitation of union encoding is that the inferred type of the literal 2-sequence is
    // Seq[Product with K with java.io.Serializable], so the test needs typed explicitly.
    test[Seq[K]](Seq(B2(2, "b2"), B1(1, "b1")))
    test[Seq[K]](Seq(B2(2, "b2")))
    test[Seq[K]](Seq(BB, B2(2, "b2"), B))
    // -------------

    val b: Node[Int] = Node(1, Node(2, Node(999, null)))
    /**
      * The encoding below works only for acyclic `Node` structures.
      * The recursive+union construction of the specification is essential for dealing with
      * `null`-terminated chains of `Node`; as is the explicit provision of the implicit parameter
      * for test (the implicit parameter cannot be resolved because the context provides two
      * valid encodings). Here `?Node[Int]*` is the stream encoding for a possibly-null `Node[Int]`, and
      * for test. Here `Node[Int]*` is the stream encoding for a `Node[Int]`. The difference in
      * size of the encoding is the extra byte used to denote the branch of the 2-union.
      */
    implicit object `?Node[Int]*` extends `2-Union*`[Node[Int], Null, Node[Int]](_.asInstanceOf[Null], _.asInstanceOf[Node[Int]])(`Null*`, `Node[Int]*`)
    implicit object `Node[Int]*`  extends `2-Case-Rec*`[Node[Int], Int, Node[Int]](Node.apply, Node.unapply)(`Int*`, `?Node[Int]*`)
    test(b)(`Node[Int]*`)                 // 16 bytes
    test(b)(`?Node[Int]*`)                // 17 bytes
    test(null: Node[Int])(`?Node[Int]*`)  // 2 bytes

    // ----------------
    try {
      /**
        * This specification is type-correct, but will fail at the moment of encoding the `null` at the end of
        * the `Node` chain.
        *
        */
      lazy val `Node[Int]WRONG`: Stream[Node[Int]] = new`2-Case-Rec*`[Node[Int], Int, Node[Int]](Node.apply, Node.unapply)(`Int*`, `Node[Int]WRONG`)
      test(b)(`Node[Int]WRONG`)
    } catch {
      case exn: Throwable => println(s"**** Encoding failure as expected:\n**** Node.unapply(null) fails at the end of the chain:\n**** ${exn.toString}")
    }


    println("--------------- Serialized -----------------")
    try {
      /**
        * This specification succeeds, because `Serializable*` can encode arbitrary object graphs,
        * whether cycic or not.
        *
        * Notice the size of the encoding is between 15x and 20x the size of the straightforward encoding(s).
        */
      object `Node[Int]**` extends `Serializable*`[Node[Int]]
      b.next.next = Node(999, b)
      test(b)(`Node[Int]**`)  // 264 bytes
      b.next.next = Node(999, null)
      test(b)(`Node[Int]**`)  // 260 bytes
    } catch {
      case exn: Throwable =>
           println("$exn during serializable roundtrip")
    }

    val f = new Foo("aFoo")
    test(f)
    test(new Foo("anotherFoo"))(new `Serializable*`[Foo])


    println("--------------- Types where == is not structural equality  -----------------")
    println("**** Expected failure report (structural equality fails at the embedded array)")
    test((1, 2, Record("Expected", 42), 3.2, Array("What do you think", "Mr Apocalypse")))
    test(Array("test tries harder for top-level arrays"))



  }
}
