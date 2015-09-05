class Rational(x: Int, y: Int) {

  require(y != 0, "Denominator must not be equal to 0.")

  def this(x: Int) = this(x, 1)

  private val g = gcd(x, y)
  def numer = x
  def denom = y

  def +(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def unary_- : Rational = new Rational(-numer, denom)

  def -(that: Rational) = this + -that

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  def < (that: Rational) =
    numer * that.denom < that.numer * denom

  def max(that: Rational) =
    if (this < that) that else this

  override def toString = s"${numer/g}/${denom/g}"

}



val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)


x - y - z
y + y
x < y
x max y

-x

//val strange = new Rational(1, 0)
//strange.add(strange)

new Rational(2)
