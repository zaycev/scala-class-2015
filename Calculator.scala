abstract class Tree

case class Sum(l: Tree, r: Tree) extends Tree
case class Var(n: String) extends Tree
case class Const(v: Int) extends Tree

type Environment = String => Int

def eval (t: Tree, env: Environment): Int = t match {
  case Sum(l, r) => eval(l, env) + eval(r, env)
  case Var(n) => env(n)
  case Const(v) => v
}


def derive(t: Tree, v: String): Tree = t match {
  case Sum(l, r)  => Sum(derive(l, v), derive(r, v))
  case Var(n) if v == n => Const(1)
  case _ => Const(0)
}

def simplify(t: Tree): Tree = t match {
  case Sum(Sum(a, b), Sum(x, y)) => simplify(Sum(simplify(Sum(a, b)), simplify(Sum(x, y))))
  case Sum(Const(l), Const(r)) => Const(l + r)
  case Sum(Const(l), r) if l == 0 => simplify(r)
  case Sum(l, Const(r)) if r == 0 => simplify(l)
  case Sum(l, r) => Sum(simplify(l), simplify(r))
  case _ => t
}


trait Ord {
  def <  (that: Any): Boolean
  def <= (that: Any): Boolean = (this < that) || (this == that)
  def >  (that: Any): Boolean = !(this <= that)
  def >= (that: Any): Boolean = !(this < that)
}


class Date (y: Int, m: Int, d: Int) extends Ord {
  def year = y
  def month = m
  def day = d

  override def toString: String = year + "-" + month + "-" + day

  override def equals(that: Any): Boolean = that.isInstanceOf[Date] && {
    val o = that.asInstanceOf[Date]
    o.day == day && o.month == month && o.year == year
  }

  def < (that: Any): Boolean = {
    if (!that.isInstanceOf[Date])
      sys.error("Cannot compare " + that + " and " + this)
    val o = that.asInstanceOf[Date]
    (year < o.year) || (year == o.year && (month < o.month || (month == o.month && day < o.day)))
  }

}


class Reference[T] {

  private var contents: T = _

  def set(value: T): Unit = {
    contents = value
  }

  def get: T = contents


}



def main(agrs: Array[String]): Unit = {
//  val exp: Tree = Sum(Sum(Var("x"), Var("x")), Sum(Const(7), Var("y")))
//  val env: Environment = { case "x" => 5 case "y" => 7 }
//  println("Expression: " + exp)
//  println("Evaluation: " + eval(exp, env))
//  println("Derivative x: " + derive(exp, "x"))
//  println("Derivative x: " + simplify(derive(exp, "x")))
//  println("Derivative y: " + derive(exp, "y"))
//  println("Derivative y: " + simplify(derive(exp, "y")))
//  println("\n\n\n")


//  println(new Date(2012, 10, 5) < new Date(2011, 10, 5))
//  println(new Date(2012, 10, 5) > new Date(2011, 10, 5))
//  println(new Date(2012, 10, 5) == new Date(2011, 10, 5))
//  println(new Date(2012, 10, 5) == new Date(2012, 10, 5))
//  println(new Date(2012, 10, 5) >= new Date(2012, 10, 5))
//  println(new Date(2012, 10, 5) <= new Date(2012, 10, 5))
//  println(new Date(2012, 10, 5) < new Date(2012, 10, 5))
//  println(new Date(2012, 10, 5) > new Date(2012, 10, 5))

  val cell = new Reference[Int]
  cell.set(100)

//  println("Cell" + cell + "   " + cell.get * 2)

  println(2 until 10 by 3)

}


main(new Array[String](0))