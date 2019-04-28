import util.UnitSpec

/**
  * Created by xsobrietyx on 27-April-2019 time 16:56
  */
class HREasyProblemsSpec extends UnitSpec {

  import HREasyProblems._

  "Compare triplets" should "return valid values" in {
    compareTriplets(Array[Int](1, 2, 3), Array[Int](3, 2, 1)) should be(Array[Int](1, 1))
    compareTriplets(Array[Int](5, 5, 6, 5, 10, 5, 7), Array[Int](9, 5, 5, 8, 5, 5, 5)) should be(Array[Int](3, 2))
  }

  "Diagonal difference" should "return valid values" in {
    diagonalDifference(Array[Array[Int]](Array[Int](1, 2, 3), Array[Int](4, 8, 9), Array[Int](3, 2, 1))) should be(4)
    diagonalDifference(Array[Array[Int]](
      Array[Int](5, 15, 25, 90, 9),
      Array[Int](34, 18, 29, 18, 2),
      Array[Int](73, 42, 11, 1, 2),
      Array[Int](31, 22, 21, 14, 12),
      Array[Int](43, 42, 24, 31, 2),
    )) should be(53)
  }
}
