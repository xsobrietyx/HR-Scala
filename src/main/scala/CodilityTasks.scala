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
      val chars = n.toString
      val res = chars
        .replaceFirst("1", "a")
        .replaceAll("\\d", "0")
        .replace('a', '1').toInt
      res
    }

  }

  /*
    This function should return a maximum depth of the successful Math.sqrt(n),
    where n is in the range of the input values.
    Example: in range from 6000 to 7000 function should return 3
   */
  def sqrtMaxDepth(a: Int, b: Int): Int = {
    import scala.annotation.tailrec
    @tailrec
    def recursiveSQRT(n: Int, acc: Int = 0): Int = {
      if (Math.sqrt(n).isValidInt) recursiveSQRT(Math.sqrt(n).toInt, acc + 1)
      else acc
    }

    var times = 0
    (a to b).foreach(n => if (recursiveSQRT(n) > 0) {
      if (times < recursiveSQRT(n)) times = recursiveSQRT(n)
    })
    times
  }
}
