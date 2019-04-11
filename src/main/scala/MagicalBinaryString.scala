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
      should return 11100100(0,7), 10(1,3) and 1100(3,7) should be replaced with each other
   */
  val input: String = "11011000"
  /*
       0 1234  56 7
      (1(1100)(10)0) => 11100100
   */

  /*
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

  def largestMagical(in: String): String = {

    var substrings: List[Tuple3[String, Int, Int]] = List()
    val splittedInput = in.split("")

    // Tests whether the string is magical ("1"'s count == "0"'s count)
    def isMagical(str: String, in: String): Boolean = {

      var sum = 0
      val splittedSubstring = str.split("")

      if (splittedSubstring.length > 1 && str != in) {
        for (k <- splittedSubstring.indices) {
          if (splittedSubstring(k) == "0") {
            sum = sum - 1
          }
          if (splittedSubstring(k) == "1") {
            sum = sum + 1
          }
        }
      }
      sum == 0
    }

    for (i <- splittedInput.indices; j <- i until splittedInput.length + 1) {

      val substring = in.substring(i, j)

      if (isMagical(substring, in) && !substrings.map(t => t._1).contains(substring)) {
        // tuples picked for better testing and tuning options during the algorithm implementation
        substrings = substrings :+ (substring, i, j)
      }
    }

    substrings
      .sortWith((a: Tuple3[String, Int, Int], b: Tuple3[String, Int, Int]) => a._1.length > b._1.length && a._1 > b._1)

    var res: (String, Boolean) = ("", false)

    for (r <- substrings; z <- substrings.reverse) {

      val possible: String = in.
        replace(r._1, "a")
        .replace(z._1, "b")
        .replace("a", z._1)
        .replace("b", r._1)

      if (possible.length == in.length && isMagical(possible, in)) res = (possible, true)
    }

    if (res._2) res._1
    else in
  }

  println(largestMagical(input)) // 11100100

}
