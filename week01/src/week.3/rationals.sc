class Rational(x: Int, y: Int) {

  require(y != 0, "Denom should not be 0")

  def this(x: Int) = this(x,1)

  private val gcdVal = gcd(x,y)
  private def gcd(x: Int, y: Int): Int = if(y == 0) x else gcd(y, x % y)

  val numer = x / gcdVal
  val denom = y / gcdVal


  def add(that: Rational): Rational = new Rational(
  numer * that.denom + that.numer * denom,
  denom * that.denom
  )

  def neg = new Rational(-numer, denom)

  def sub(that: Rational) = add(that.neg)

  override def toString: String = numer + "/" + denom

}
  val x = new Rational(14,21)
  val y = new Rational(3)
