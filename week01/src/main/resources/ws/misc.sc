def init[T](list: List[T]): List[T] = list match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

def reverse[T](list: List[T]): List[T] = list match {
  case List() => list
  case y :: ys => reverse(ys) ::: List(y)
}

def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
  case List() => List()
  case _ :: tail if n == 0 => tail
  case head :: tail => head :: removeAt(n - 1, tail)
}

def flatten(xs: List[Any]): List[Any] = xs match {
  case List() => List()
  case (x: List[Any]) :: tail => flatten(x) ::: flatten(tail)
  case x :: tail => x :: flatten(tail)
}


def msort(list: List[Int]): List[Int] = {

  def merge(left: List[Int], right: List[Int]): List[Int] = (left, right) match {
    case (Nil, r) => r
    case (l, Nil) => l
    case (lHead :: lTail, rHead :: rTail) =>
      if (lHead <= rHead) lHead :: merge(lTail, right) else rHead :: merge(left, rTail)
  }

  val idx = list.length / 2
  if (idx == 0) list
  else {
    val (left, right) = list splitAt idx
    merge(msort(left), msort(right))
  }
}

init(List(1, 2, 3, 4))

reverse(List(1, 2, 3, 4))

removeAt(0, List(1, 2, 3, 4))

removeAt(2, List(1, 2, 3, 4))

removeAt(5, List(1, 2, 3, 4))

msort(List(1, 4, 5, 6, 4, 100, 3, 7, 6, -777 ))

flatten(List(List(1, 1), 2, List(3, List(5, 8))))

