import scala.annotation.tailrec

/**
  * Created by xsobrietyx on 27-April-2019 time 15:44
  */
object HREasyProblems extends App {
  /*
  Alice and Bob each created one problem for HackerRank. A reviewer rates the two challenges, awarding points on a scale from 1 to 100 for three categories: problem clarity, originality, and difficulty.
  We define the rating for Alice's challenge to be the triplet a = (a[0],a[1],a[2]) , and the rating for Bob's challenge to be the triplet b = (b[0],b[1],b[2]).
  Your task is to find their comparison points by comparing a[0] with b[0], a[1] with b[1], and a[2] with b[2].
  If a[i] > b[i], then Alice is awarded  point.
  If a[i] < b[i], then Bob is awarded  point.
  If a[i] == b[i], then neither person receives a point.
  Comparison points is the total points a person earned.
  Given a and b, determine their respective comparison points.
  For example, a = [1,2,3] and b = [3,2,1]. For elements 0, Bob is awarded a point because a[0] < b[0]. For the equal elements a[1] and b[1], no points are earned. Finally, for elements 2, a[2] > b[2] so Alice receives a point. Your return array would be  with Alice's score first and Bob's second.
   */
  def compareTriplets(a: Array[Int], b: Array[Int]): Array[Int] = {
    var res: (Int, Int) = (0, 0)

    a.zip(b).foreach(x => {
      if (x._1 > x._2) res = (res._1 + 1, res._2) else if (x._2 > x._1) res = (res._1, res._2 + 1)
    })
    Array(res._1, res._2)
  }

  /*
  Given a square matrix, calculate the absolute difference between the sums of its diagonals.
  For example, the square matrix is shown below:
  1 2 3
  4 5 6
  9 8 9
  For this particular case result should be: |(1 + 5 + 9) - (9 + 5 + 3)| = |15 - 17| = 2
   */
  def diagonalDifference(matrix: Array[Array[Int]]): Int = {
    val i1: (Int, Int) = (0, 0)
    val i2: (Int, Int) = (matrix.length - 1, 0)

    @tailrec
    def compute(iter0: Int = 0, iter1: (Int, Int), iter2: (Int, Int), result: (Int, Int) = (0, 0)): Int = {
      if (iter0 == matrix.length) Math.abs(result._1 - result._2)
      else compute(iter0 + 1,
        (iter1._1 + 1, iter1._2 + 1),
        (iter2._1 - 1, iter2._2 + 1),
        (result._1 + matrix(iter1._1)(iter1._2), result._2 + matrix(iter2._1)(iter2._2)))
    }

    compute(iter1 = i1, iter2 = i2)
  }

  /*
  Given an array of integers, calculate the fractions of its elements that are positive, negative, and are zeros. Print the decimal value of each fraction on a new line.
   */
  def countPercents(arr: Array[Int]): Unit = {
    var res = (0.0, 0.0, 0.0)
    arr.foreach {
      case a: Int if a > 0 => res = (res._1 + 1, res._2, res._3)
      case a: Int if a < 0 => res = (res._1, res._2 + 1, res._3)
      case a: Int if a == 0 => res = (res._1, res._2, res._3 + 1)
    }
    val length = arr.length.toDouble

    println(res._1 / length)
    println(res._2 / length)
    println(res._3 / length)
  }

  /*
  Consider a staircase of size n = 4:
     #
    ##
   ###
  ####
  Observe that its base and height are both equal to n, and the image is drawn using # symbols and spaces. The last line is not preceded by any spaces.
  Write a program that prints a staircase of size n.
  */

  def staircase(n: Int): Unit = {
    (1 to n).foreach(z => {
      (1 to n - z).foreach(_ => print(" "))
      (1 to z).foreach(_ => print("#"))
      println("")
    })
  }

  /*
Given five positive integers, find the minimum and maximum values that can be calculated by summing exactly four of the five integers.
Then print the respective minimum and maximum values as a single line of two space-separated long integers.
In example below output should be: 16 23
1 8 3 5 = 17
1 8 3 7 = 19
1 8 5 7 = 21
1 3 5 7 = 16
8 3 5 7 = 23
For input Array[BigInt](256741038,623958417,467905213,714532089,938071625)
Result should be: 2063136757 2744467344
This particular task was a little bit tricky, because HR platform gives a Scala template with signature of function:
  def minMaxSum(arr: Array[Int]): Unit
and input parsing in main method got the next state:
    **DEFAULT**
    def main(args: Array[String]) {
        val stdin = scala.io.StdIn

        val arr = stdin.readLine.split(" ").map(_.trim.toInt)
        miniMaxSum(arr)
    }
    ***********
Which is not adopted for the proper test cases/input values. So I need first to correct this in such way:
    **CORRECT**
        def main(args: Array[String]) {
        val stdin = scala.io.StdIn

        val arr: Array[BigInt] = stdin.readLine.split(" ").map(v => BigInt(v.trim.toInt))
        miniMaxSum(arr)
    }
    *******************************
 */
  def miniMaxSum(arr: Array[BigInt]): Unit = {
    /*
    Shorthand solution that works in case of input array is a set of integer values
    println(s"${arr.filter(e => e != arr.max).sum} ${arr.filter(e => e != arr.min).sum}")
     */
    @tailrec
    def findMin(counter: Int = 0, res: BigInt): BigInt = {
      if (counter == arr.length) res
      else {
        val currentResult: BigInt = arr.zipWithIndex.filter(i => i._2 != counter).map(t => t._1).sum
        val nextRes: BigInt = if (currentResult < res) currentResult else res
        findMin(counter + 1, nextRes)
      }
    }
    @tailrec
    def findMax(counter: Int = 0, res: BigInt = 0): BigInt = {
      if (counter == arr.length) res
      else {
        val currentResult: BigInt = arr.zipWithIndex.filter(i => i._2 != counter).map(t => t._1).sum
        val nextRes: BigInt = if (currentResult > res) currentResult else res
        findMax(counter + 1, nextRes)
      }
    }

    println(s"${findMin(res = arr.sum)} ${findMax()}")
  }
}
