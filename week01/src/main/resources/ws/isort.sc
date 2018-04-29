def isort(list: List[Int]): List[Int] = list match {
  case List() => List()
  case x :: xs => insert(x, isort(xs))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if(x < y) x :: xs else y :: insert(x, ys)
}

isort(List(1,2,3,4,5,6))

isort(List(6,5,8,8,4,3,3,2,1))