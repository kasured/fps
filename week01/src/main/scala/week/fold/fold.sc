def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())(f(_) :: _)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((elem: T, zero: Int) => zero + 1)

mapFun(List(1,2,3), (elem: Int) => elem * 2)

lengthFun(List(1,2,3))

