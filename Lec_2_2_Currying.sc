object session {

  def sum(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int =
      if (a > b) 0
      else f(a) + sumF(a + 1, b)
    sumF
  }


  def sumInts = sum(x => x)

  def sumSqrs = sum(x => x * x)

  sumInts(1, 10) + sumSqrs(1, 100)

  sum(x => x * x)(1, 10) == sumSqrs(1, 10)

  {
    def sum(f: Int => Int)(a: Int, b: Int): Int =
      if (a > b) 0 else f(a) + sum(f)(a + 1, b)

    sum(x => x * x)(1, 10)
  }


  // Some lisp Fun.

  def Tuple(a: Any, b: Any): ((Any, Any) => Any) => Any = (g: (Any, Any) => Any) => g(a, b)

  def Head(a: Any, b: Any): Any = a

  def Tail(a: Any, b: Any): Any = b

  val t1 = Tuple(1, 2)
  t1(Head)
  t1(Tail)
  val list = Tuple(1, Tuple(2, Tuple(3, 4)))
  val head = list(Head)

  // 1.
  def prod(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1 else f(a) * prod(f)(a + 1, b)

  prod(x => x * x)(3, 4)

  // 2.
  def fact(n: Int): Int = prod(x => x)(1, n)

  fact(5)

  // 3.
  def mapReduce(m: Int => Int, r: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else r(m(a), mapReduce(m, r, zero)(a + 1, b))

  def mrSum(f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(f, (x, y) => x + y, 0)(a: Int, b: Int)

  def mrProd(f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(f, (x, y) => x * y, 1)(a: Int, b: Int)

  mrSum(x => x)(0, 3)

  mrProd(x => x)(1, 3)

}
