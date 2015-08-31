import scala.annotation.tailrec

// Lecture 1.7

object session {

  @tailrec
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  gcd(14, 21)

  def factorial(n: Int): Int = {

    @tailrec
    def factorial_tail(n: Int, acc: Int): Int =
      if (n <= 1) acc else factorial_tail(n - 1, n * acc)

    factorial_tail(n, 1)

  }

  factorial(4)
  factorial(15)

}
