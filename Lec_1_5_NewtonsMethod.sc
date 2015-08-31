// Lecture 1.5

object session {

  def abs (x: Double) = if (x < 0) -x else x

  abs(-1)
  abs(-100)
  abs(100)

  def sqrt(x: Double) = {

    def sqrtIter(guess: Double, x: Double): Double =
      if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess, x), x)


    def isGoodEnough(guess: Double, x: Double) =
      abs(guess * guess - x) / x < 0.01

    def improve(guess: Double, x: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0, x)

  }

  sqrt(2)
  sqrt(4)
  sqrt(1e-6)
  sqrt(1e60)

}


