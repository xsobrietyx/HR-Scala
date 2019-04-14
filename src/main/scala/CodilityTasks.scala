/**
  * Created by xsobrietyx on 13-April-2019 time 23:20
  */
object CodilityTasks {

  /*
      Solution should return a minimal value of integer for the same integer length as a result input.
      Examples: 125 should return 100
                10 should return 10
                1 should return 0
   */
  def minimalDigits(n: Int): Int = {
    if (n == 1) 0

    else {
      val asString = n.toString
      val res = asString
        .replaceFirst("1", "a")
        .replaceAll("\\d", "0")
        .replace('a', '1').toInt
      res
    }

  }

  /*
    This function should return a maximum depth of the successful Math.sqrt(n),
    where n is in the range of the input values.
    Example:  in range from 0 to 20 function should return 2
              sqrt(16) -> 4, sqrt(4) -> 2
              in range from 6000 to 7000 function should return 3
              sqrt(6561) -> 81, sqrt(81) -> 9, sqrt(9) -> 3
              in range from 100 to 70000 function should return 4
              sqrt(65536) -> 256, sqrt(256) -> 16, sqrt(16) -> 4, sqrt(4) -> 2
   */
  def sqrtMaxDepth(a: Int, b: Int): Int = {
    import scala.annotation.tailrec
    @tailrec
    def recursiveSQRT(n: Int, acc: Int = 0): Int = {
      if (Math.sqrt(n).isValidInt) recursiveSQRT(Math.sqrt(n).toInt, acc + 1)
      else acc
    }

    var times = 0
    (a to b).filter(n => n != 0 && n != 1).foreach(n => if (recursiveSQRT(n) > 0) {
      val potential = recursiveSQRT(n)
      if (times < potential) times = potential
    })
    times
  }
}
