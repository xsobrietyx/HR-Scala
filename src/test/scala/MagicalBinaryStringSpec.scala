import util.UnitSpec

/**
  * Created by xsobrietyx on 11-April-2019 time 21:56
  */
class MagicalBinaryStringSpec extends UnitSpec {

  "MagicBinaryString.largestMagical()" should "return proper magical strings" in {
    val magicalBS = new MagicalBinaryString()

    magicalBS.largestMagical("11011000") should be ("11100100")
    magicalBS.largestMagical("1100") should be ("1100")
    magicalBS.largestMagical("1101001100") should be ("1101001100")
    magicalBS.largestMagical("1010") should be ("1010")

    magicalBS.largestMagical("10") should be ("10")
    magicalBS.largestMagical("0") should be ("0")
    magicalBS.largestMagical("1") should be ("1")
    magicalBS.largestMagical("") should be ("")
  }

}
