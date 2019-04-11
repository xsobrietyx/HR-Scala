import util.UnitSpec

/**
  * Created by xsobrietyx on 11-April-2019 time 21:56
  */
class MagicalBinaryStringSpec extends UnitSpec {

  "MagicBinaryString.largestMagical()" should "return proper magical strings" in {
    val magicalBinaryString = new MagicalBinaryString()

    assert(magicalBinaryString.largestMagical("11011000") == "11100100")
    assert(magicalBinaryString.largestMagical("1100") == "1100")
    assert(magicalBinaryString.largestMagical("1101001100") == "1101001100")
  }

}
