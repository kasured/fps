package week.four

import scala.annotation.tailrec

/**
  * Created by kasured on 9/13/16.
  */
trait List[+T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]

  def prepend[U >: T](elem: U): List[U] = Cons(elem, this)

  def foldLeft[U](zero: U)(op: (U, T) => U): U = this match {
    case Nil => zero
    case Cons(x, xs) => (xs foldLeft op(zero, x)) (op)
  }

}

case class Cons[T](head: T, tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}

object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException("Nil.head")

  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object List {

  def apply[T]: List[T] = Nil

  def apply[T](elem: T): List[T] = Cons(elem, apply)

  def apply[T](first: T, second: T): List[T] =  Cons(first, apply(second))

  def singleton[T](elem: T) = new Cons[T](elem, Nil)

  def take[T](list: List[T], n: Int): T = {
    if (n < 0 || list.isEmpty) throw new IndexOutOfBoundsException

    @tailrec
    def _take(list: List[T], counter: Int): T = {
      if (list.isEmpty) throw new IndexOutOfBoundsException
      else if (counter + 1 == n) list.head
      else _take(list.tail, counter + 1)
    }

    _take(list, 0)
  }

}