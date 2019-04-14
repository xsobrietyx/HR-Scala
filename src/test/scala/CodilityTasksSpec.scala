import CodilityTasks.{minimalDigits, sqrtMaxDepth}
import util.UnitSpec

/**
  * Created by xsobrietyx on 13-April-2019 time 23:49
  */
class CodilityTasksSpec extends UnitSpec {
  "Minimal digits" should "return a minimal value of integer for the same integer length" in {
    minimalDigits(113424) should be (100000)
    minimalDigits(1189) should be (1000)
    minimalDigits(125) should be (100)
    minimalDigits(10) should be (10)
    minimalDigits(18) should be (10)
    minimalDigits(1) should be (0)
    minimalDigits(8) should be (0)
  }

  "SqrtMaxDepth" should "return a maximum depth of the successful Math.sqrt(n) in range that passed as arguments" in {
    sqrtMaxDepth(0, 20) should be (2)
    sqrtMaxDepth(6000, 7000) should be (3)
    sqrtMaxDepth(100, 70000) should be (4)
  }
}
