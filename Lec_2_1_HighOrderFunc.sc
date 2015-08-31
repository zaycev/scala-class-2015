import scala.annotation.tailrec


// Lecture 2.1

object session {

  def sum(f: Int => Int, a: Int, b: Int): Int = {
    @tailrec
    def sum_rec(a: Int, b: Int, acc: Int): Int =
      if (a > b) acc else sum_rec(a + 1, b, acc + f(a))
    sum_rec(a, b, 0)
  }

  val s = {
    val f = (x: Int) => x * x - x
    sum(f, 1, 4)
  }

}
