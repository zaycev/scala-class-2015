import math.abs
import scala.annotation.tailrec

object session {


  def findFixedPoint(f: Double => Double) (firstGuess: Double = 1.0, error: Double = 0.00001) = {
    def isCloseEnough(x: Double, y: Double) =
      abs((x - y) / x) / x < error
    @tailrec
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2


  def sqrt(x: Double) =
    findFixedPoint(averageDamp(y => x / y))(1.0)

  findFixedPoint(x => 1 + x / 2)(1.0)


  sqrt(2)
  sqrt(4)
  sqrt(9)
  sqrt(144)
  sqrt(7)

}
