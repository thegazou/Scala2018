package lessons.lesson3

object Rational {

  class Rational(n: Int, d: Int) {
    private val g = gcd(n, d)

    val num: Int = n / g
    val denom: Int = d / g

    def minus(that: Rational): Rational = {
      new Rational(num * that.denom + that.num * denom, denom * that.denom)
    }

    def add(that: Rational): Rational = {
      new Rational(num * that.denom + that.num * denom, denom * that.denom)

    }

    private def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)
  }

}
