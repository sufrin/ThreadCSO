package ox.net.codec

object StreamEncodingShortTests {
  import DataStreamEncoding._

  trait K
  case class B1(n: Int, s: String) extends K
  case class B2(n: Int, s: String) extends K
  case object B extends K
  case object BB extends K

  object Day extends Enumeration {
    val Mon, Tue, Wed = Value
  }
  type Day = Day.Value

  case class Record(name: String, value: Int)

  /**  The implicit encodings below are named conventionally
    *  after the types they encode. This is not mandatory, but
    *  it helps to construct the encodings systematically, and
    *  to make an existing corpus of encodings complete.
    */
  implicit object `(String,Int)*`       extends `2-Tuple*`[String, Int]
  implicit object `Record*`             extends `2-Case*`[Record, String, Int](Record.apply, Record.unapply)
  implicit object `Seq[Int]*`           extends `Seq*`[Int]
  implicit object `Seq[(String,Int)]*`  extends `Seq*`[(String, Int)]
  implicit object `Seq[String]*`        extends `Seq*`[String]
  implicit object `List[String]*`       extends `List*`[String]
  implicit object `Array[String]*`      extends `Array*`[String]
  implicit object `Seq[Record]*`        extends `Seq*`[Record]
  implicit object `Day*`                extends `Enum*`[Day](Day.apply, _.id)

  implicit object `Seq[Day]*`           extends `Seq*`[Day]
  implicit object `Set[Day]*`           extends `Set*`[Day]
  implicit object `Option[Day]*`        extends `Option*`[Day]
  implicit object `(Int,Int,Record,Double,Seq[String],Seq[Int])*`  extends `6-Tuple*`[Int, Int, Record, Double, Seq[String], Seq[Int]]
  implicit object `(Int,Int,Record,Double,List[String])*`          extends `5-Tuple*`[Int, Int, Record, Double, List[String]]
  implicit object `(Option[Day],Int,Record,Double,Array[String])*` extends `5-Tuple*`[Option[Day], Int, Record, Double, Array[String]]

  implicit object `B1*` extends `2-Case*`[B1, Int, String](B1.apply, B1.unapply)
  implicit object `B2*` extends `2-Case*`[B2, Int, String](B2.apply, B2.unapply)
  implicit object `B*`  extends `Case*`[B.type](B)
  implicit object `BB*` extends `Case*`[BB.type](BB)

  implicit object `K*` extends `4-Union*`[K, BB.type, B1, B2, B.type](asObject(BB), _.asInstanceOf[B1],  _.asInstanceOf[B2], asObject(B))
  implicit object `Seq[K]*` extends `Seq*`[K]

  implicit object `(Int,Int,Record,Double,Array[String])*`
      extends `5-Tuple*`[Int, Int, Record, Double, Array[String]]
  implicit object `(Int,Int,Record,Double,Seq[String])*`
      extends `5-Tuple*`[Int, Int, Record, Double, Seq[String]]
  implicit object `(Option[Day],Int,Record,Double,List[String])*`
      extends `5-Tuple*`[Option[Day], Int, Record, Double, List[String]]
  implicit object `(Option[Day],Int,Record,Double,Seq[String])*)`
      extends `5-Tuple*`[Option[Day], Int, Record, Double, Seq[String]]

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
    test(Array("What do you think", "Mr Apocalypse"))
    // A limitation of union encoding: the inferred type of the literal 2-sequence is
    // Seq[Product with K with java.io.Serializable]
    test[Seq[K]](Seq(B2(2, "b2"), B1(1, "b1")))
    test[Seq[K]](Seq(B2(2, "b2")))
    test[Seq[K]](Seq(BB, B2(2, "b2"), B))
    // -------------

    //
    println(
      "****\n**** Expected roundtrip failure(s)\n**** (Structures containing arrays fail the == test (but are still equivalent)\n****"
    )
    test(
      (
        1,
        2,
        Record("Expected", 42),
        3.2,
        Array("What do you think", "Mr Apocalypse")
      )
    )
  }

}
