import MagicalBinaryString.largestMagical
import util.UnitSpec

/**
  * Created by xsobrietyx on 11-April-2019 time 21:56
  */
class MagicalBinaryStringSpec extends UnitSpec {

  "MagicBinaryString.largestMagical()" should "return proper magical strings" in {
    largestMagical("11011000") should be ("11100100")
    largestMagical("1100") should be ("1100")
    largestMagical("1101001100") should be ("1101001100")
    largestMagical("1010") should be ("1010")

    largestMagical("10") should be ("10")
    largestMagical("0") should be ("0")
    largestMagical("1") should be ("1")
    largestMagical("") should be ("")
  }

}
