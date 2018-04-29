package week.four

/**
  * Created by kasured on 9/15/16.
  */
abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat = new Succ(this)

  def +(that: Nat): Nat

  def -(that: Nat): Nat
}

object Zero extends Nat {

  override def isZero: Boolean = true

  override def predecessor: Nat = throw new IllegalArgumentException("Zero.predecessor")

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if (that.isZero) Zero else throw new IllegalArgumentException("Zero.minus(nonZero)")
}

class Succ(n: Nat) extends Nat {

  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def +(that: Nat): Nat =
    if (that.isZero) this else successor + that.predecessor

  override def -(that: Nat): Nat =
    if (that.isZero) this else predecessor - that.predecessor
}
