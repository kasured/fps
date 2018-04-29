package week.four

/**
  * Created by kasured on 9/16/16.
  */
trait Expr

case class Number(n: Int) extends Expr

case class Sum(left: Expr, right: Expr) extends Expr

case class Var(name: String) extends Expr

case class Prod(left: Expr, right: Expr) extends Expr

object Expr {
  def show(e: Expr): String  = e match {
    case Number(n) => n.toString
    case Sum(left, right) => "(" + show(left) + " + " + show(right) + ")"
    case Var(name) => name
    case Prod(left, right) => show(left) + " * " + show(right)
  }
}
