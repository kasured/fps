object product {
  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if(a > b) 1 else f(a) * product(f)(a + 1, b)
  }

  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    if(a > b) 0 else f(a) + sum(f)(a + 1, b)
  }

  def factorial(n: Int): Int = {
    product(x => x)(1,n)
  }


  def general(f: Int => Int)(acc: (Int, Int) => Int)(unit: Int)(a: Int, b: Int): Int = {
    if(a > b) unit
    else acc(f(a), general(f)(acc)(unit)(a + 1, b))
  }

  def product1(f: Int => Int)(a: Int, b: Int): Int = general(f)((x,y) => x * y)(1)(a,b)

  def sum1(f: Int => Int)(a: Int, b: Int): Int = general(f)((x,y) => x + y)(0)(a,b)

  product(x => x)(1,5)
  product1(x => x)(1,5)

  sum(x => x)(1,5)
  sum1(x => x)(1,5)

  factorial(5)
}