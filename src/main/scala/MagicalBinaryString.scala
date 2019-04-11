/*
We define the following:
A binary string is a string consisting only of 0's and/or 1's. For example, 01011, 1111, and 00 are all binary strings.
The prefix of a string is any substring of the string that includes the beginning of the string. For example, the prefixes of 11010 are 1, 11, 110, 1101, and 11010.

We consider a non-empty binary string to be magical if the following two conditions are true:
The number of 0's is equal to the number of 1's.
For every prefix of the binary string, the number of 1's should not be less than the number of 0's.
For example, 11010 is not magical because it doesn't have an equal number of 0's and 1's, but 110100 is magical because it satisfies both of the above conditions.

A magical string can contain multiple magical substrings. If two consecutive substrings are magical, then we can swap the substrings as long as the resulting string is still a magical string. Given a magical binary string, str, perform zero or more swap operations on its consecutive magical substrings such that the resulting string is as lexicographically large as possible. Two substrings are considered to be consecutive if the last character of the first substring occurs exactly one index before the first character of the second substring.

Complete the largestMagical function in the editor below. It has 1 parameter: a string, str. It must return a string denoting the lexicographically largest possible magical string that can be formed by performing zero or more swap operations on consecutive magical substrings of str.

Input Format
Locked stub code in the editor reads a single binary string, str, from stdin and passes it to the function.

Constraints
It is guaranteed that str is a binary string of 1's and 0's only.
1 ≤ length(str) ≤ 50
It is guaranteed that str is a magical string.

Output Format
The function must return a string denoting the lexicographically largest magical string that can be formed from str. This is printed to stdout by locked stub code in the editor.

Sample Input 0
11011000

Sample Output 0
11100100

Explanation 0
Given the magical string str = 11011000, we can choose two consecutive magical substrings, 1100 and 10, to swap such that the resultant string, str' = 11100100, is the lexicographically largest possible magical string possible. Thus, we return the value of str', which is 11100100, as our answer.

Sample Input 1
1100

Sample Output 1

1100

Explanation 1
The only magical substring of str is 1100. So none of the operations can be applied on the string.

Sample Input 2
1101001100

Sample Output 2
1101001100

Explanation 2
The only consecutive magical substrings of str are 110100 and 1100 (note that 100 is not a magical substring because it contains more zeroes than ones); if we were to swap them, it would result in a lexicographically smaller string. Thus, str is already the lexicographically largest magical string that can be formed and we return 1101001100 as our answer.

--note that the concatenation of any two magical strings is magical. also any prefix of a magical string meeting cond 1 is magical

 */

/**
  * Created by xsobrietyx on 10-April-2019 time 20:19
  */
object MagicalBinaryString extends App {
  /*
      f(input_1) should return 11100100(0,7), 10(1,3) and 1100(3,7) should be replaced with each other
   */
  val input_1: String = "11011000"
  val input_2: String = "1100"
  val input_3: String = "1101001100"
  /*
       0 1234  56 7
      (1(1100)(10)0) => 11100100
   */

  /*
      Some data visualisation for input_1

      1
      11
      110
      1101
      11011
      110110
      1101100
      11011000

      1
      10 <-
      101
      1011
      10110
      101100
      1011000

      0
      01
      011
      0110
      01100
      011000

      1
      11
      110
      1100 <-
      11000

      1
      10
      100
      1000

      0
      00
      000

      0
      00

      0
   */

  def largestMagical(input: String): String = {

    def isMagical(str: String, initialString: String): Boolean = {
      case class TemporarySum(balance: Int, changed: Boolean, zeros: Int, ones: Int)
      var sum = TemporarySum(0, changed = false, 0, 0)
      val splittedSubstring = str.toCharArray

      if (splittedSubstring.length > 1 && str != initialString) {
        for (k <- splittedSubstring.indices) {
          if (splittedSubstring(k) == '0') {
            sum = TemporarySum(sum.balance - 1, changed = true, sum.zeros + 1, sum.ones)
          }
          if (splittedSubstring(k) == '1') {
            sum = TemporarySum(sum.balance + 1, changed = true, sum.zeros, sum.ones + 1)
          }
        }
      }
      if (sum.balance == 0 && sum.changed && sum.zeros == sum.ones) true
      else false
    }

    case class Substring(value: String, startIndex: Int, endIndex: Int, initialString: String)

    def toUniqueSubstrings(inputString: String): List[Substring] = {
      val strAsArray: Array[Char] = inputString.toCharArray
      var substrings: List[Substring] = List()

      for (i <- strAsArray.indices; j <- i until strAsArray.length + 1) {
        val currentSubstring = inputString.substring(i, j)
        if (isMagical(currentSubstring, inputString) && !substrings.map(sub => sub.value).contains(currentSubstring)) {
          substrings = substrings :+ Substring(currentSubstring, i, j, inputString)
        }
      }

      substrings
    }

    def sortSubstringsList(list: List[Substring]): List[Substring] = {
      var res = list
      res.sortWith((a: Substring, b: Substring) => a.value.length > b.value.length && a.value > b.value)
      res
    }

    case class MagicalResult(value: String, ready: Boolean, initialString: String)

    def makeReplacements(list: List[Substring]): MagicalResult = {
      var res: MagicalResult = MagicalResult("", ready = false, list.head.initialString)

      if (list.nonEmpty) {
        import util.control.Breaks._
        breakable {
          for (r <- list; z <- list.reverse) {
            var solution = res.initialString

            if (r.endIndex == z.startIndex && r.value.length < z.value.length) {
              solution = solution.
                replaceFirst(r.value, "a")
                .replaceFirst(z.value, "b")
                .replaceFirst("a", z.value)
                .replaceFirst("b", r.value)
            }

            if (solution.length == res.initialString.length) res = MagicalResult(solution, ready = true, res.initialString)
            if (res.ready) break
          }
        }
      }
      res
    }

    def getAnswer(magicalResult: MagicalResult): String = {
      if (magicalResult.ready) magicalResult.value
      else magicalResult.initialString
    }

    getAnswer(makeReplacements(sortSubstringsList(toUniqueSubstrings(input))))
  }

  println(largestMagical(input_1)) // result => 11100100
  println(largestMagical(input_2)) // result => 1100
  println(largestMagical(input_3)) // result => 1101001100

}
