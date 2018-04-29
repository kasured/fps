package week

/**
  * Created by kasured on 9/20/16.
  */
package object four {

  import scala.List

  import scala.Nil

  /**
    * Merge sort implementation
    *
    * @param list - list to be sorted
    * @return new sorted list in ascending order
    */
  def msort[T](list: List[T])(implicit ord: Ordering[T]): List[T] = {

    def merge(left: List[T], right: List[T]): List[T] = (left, right) match {
      case (Nil, r) => r
      case (l, Nil) => l
      case (lHead :: lTail, rHead :: rTail) =>
        if (ord.lteq(lHead, rHead)) lHead :: merge(lTail, right)
        else rHead :: merge(left, rTail)
    }

    val middle = list.length / 2
    if (middle == 0) list
    else {
      val (left, right) = list splitAt middle
      merge(msort(left), msort(right))
    }

  }

  def isort[T](list: List[T])(implicit ord: Ordering[T]): List[T] = {
    def insert(item: T, list: List[T]): List[T] = list match {
      case Nil => List(item)
      case head :: tail =>
        if (ord.lteq(item, head)) item :: list else head :: insert(item, tail)
    }

    if (list.isEmpty) Nil else insert(list.head, isort(list.tail))

  }


  def pack[T](list: List[T]): List[List[T]] = list match {
    case Nil => Nil
    case head :: tail =>
      val (first, rest) = list span (elem => elem == head)
      first :: pack(rest)
  }

  def encode[T](list: List[T]): List[(T, Int)] = pack(list) map (xs => (xs.head, xs.length))

}
