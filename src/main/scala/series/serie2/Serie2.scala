package series.serie2

import scala.annotation.tailrec

object Serie2 extends App {

  def fib(x: Int): Int = {
    if (x == 0)
      0
    else if (x == 1)
      1
    else
      fib(x - 1) + fib(x - 2)
  }

  def fibrec(x: Int): (Int, Int) = {
    var n = 0

    @tailrec
    def iter(i: Int = 2, s0: Int = 0, s1: Int = 1): Int = {
      n += 1
      if (i == x)
        s0 + s1
      else
        iter(i + 1, s1, s0 + s1)
    }

    if (x < 2) x
    (iter(), n)

  }
  print("fibonaci(8) = " + fibrec(8)._1 + " ")
  println("(itérations: " + fibrec(8)._2 + ")")
  println()

  def sum(f: Int => Double, a: Int, b: Int): Double = {
    @tailrec
    def iter(a: Int, acc: Double): Double = {
      if (a > b) acc
      else iter(a + 1, acc + f(a))
    }

    iter(a, 0)
  }

  print("Sum of 2x in interval 2, 4 = ")
  println(sum(x => 2 * x, 2, 4))

  /*-------------------------------------------------------------------------------------------------------
                                      General function
   -----------------------------------------------------------------------------------------------------*/
  def sum2(f: Int => Double, a: Int, b: Int): Double = {
    @tailrec
    def iter(a: Int, acc: Double): Double = {
      if (a > b) acc
      else iter(a + 1, acc + f(a))
    }

    iter(a, 0)
  }

  print("Sum of 2x in interval 2, 4 = " + sum2(x => 2 * x, 2, 4))
  println(", Expected: " + (2 * 2 + 2 * 3 + 2 * 4))

  def product(f: Int => Int, a: Int, b: Int): Int = {
    @tailrec
    def iter(i: Int = a, s: Int = 1): Int = {
      if (i > b)
        s
      else
        iter(i + 1, s * f(i))
    }

    iter()
  }

  def f(x: Int): Int = 2 * x

  print("Product of 2x between 2 and 4 = " + product(f _, 2, 4))
  println(", Expected: " + (2 * 2 * 2 * 3 * 2 * 4))

  def fact(n: Int): Int = product(x => x, 1, n)

  print("Factorial of 5 = " + fact(5))
  println(", Expected: " + (1 * 2 * 3 * 4 * 5))

  def generalFunction(f: Int => Int, a: Int, b: Int, op: (Int, Int) => Int, s: Int): Int = {
    @tailrec
    def iter(i: Int = a, result: Int = s): Int = {
      if (i > b)
        result
      else
        iter(i + 1, op(result, f(i)))
    }

    iter()
  }

  println("\nGeneralFunction")
  print("Product of 2x between 2 and 4 = " + generalFunction((x => 2 * x), 2, 4, ((x, y) => x * y), 1))
  println(", Expected: " + (2 * 2 * 2 * 3 * 2 * 4))
  print("Sum of 2x in interval 2, 4 = " + generalFunction((x => 2 * x), 2, 4, ((x, y) => x + y), 0))
  println(", Expected: " + (2 * 2 + 2 * 3 + 2 * 4))

  /*-------------------------------------------------------------------------------------------------------
                                      Curry
   -----------------------------------------------------------------------------------------------------*/
  def curry(a: Int)(b: Int)(f: Int => Double = x => x)(op: (Double, Double) => Double)(s: Int): Double = {
    @tailrec
    def iter(i: Int = a, result: Double = s): Double = {
      if (i > b)
        result
      else
        iter(i + 1, op(result, f(i)))
    }

    iter()
  }

  val sum = curry(_: Int)(_: Int)(_: Int => Double)((x, y) => x + y)(0)
  val product = curry(_: Int)(_: Int)(_: Int => Double)((x, y) => x * y)(1)
  val fact2 = curry(1)(_: Int)(x => x)((x, y) => x * y)(1)

  println("Without val function\n")
  print("Product of 2x between 2 and 4 = " + curry(2)(4)(x => 2 * x)((x, y) => x * y)(1))
  println(", Expected: " + (2 * 2 * 2 * 3 * 2 * 4))
  print("Sum of 2x in interval 2, 4 = " + curry(2)(4)(x => 2 * x)((x, y) => x + y)(0))
  println(", Expected: " + (2 * 2 + 2 * 3 + 2 * 4))

  println("\nWith val function\n")
  print("Sum of 2x in interval 2, 4 = " + sum(2, 4, x => 2 * x))
  println(", Expected: " + (2 * 2 + 2 * 3 + 2 * 4))
  print("product of 2x in interval 2, 4 = " + product(2, 4, x => 2 * x))
  println(", Expected: " + (2 * 2 * 2 * 3 * 2 * 4))
  print("factorial of 7 = " + fact2(7))
  println(", Expected: " + (1 * 2 * 3 * 4 * 5 * 6 * 7))

  //
}