package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {

    assert(c <= r, s"column $c cannot be greater than row $r")

    assert(c >= 0 && r >= 0, "parameters cannot be negative")

    def _pascal(c: Int, r: Int): Int = (c, r) match {
      case (0, _) => 1
      case (j, i) if j == i => 1
      case (j, i) => _pascal(j - 1, i - 1) + _pascal(j, i - 1)
    }

    _pascal(c, r)

  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def _balance(xs: List[Char], diff: Int): Boolean = xs match {
      case Nil => diff == 0
      case ')' :: tail if diff == 0 => false
      case ')' :: tail => _balance(tail, diff - 1)
      case '(' :: tail => _balance(tail, diff + 1)
      case head :: tail => _balance(tail, diff)
    }

    _balance(chars, 0)

  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def _countChange(money: Int, coins: List[Int]): Int = money match {
      case value if money < 0 => 0
      case value if coins.isEmpty => if(money == 0) 1 else 0
      case value => _countChange(money, coins.tail) + _countChange(money - coins.head, coins)
    }
    if (money <= 0) 0 else _countChange(money, coins)
  }
}
