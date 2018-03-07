package lessons.lesson2

object Cour2 extends App {
  def sum(f: Int => Double, a: Int, b: Int): Double = {
    if (a > b) 0
    else f(a) + sum(f, a + 1, b)
  }

  def sumInts(a: Int, b: Int): Double =
    sum((x: Int) => x, a, b)

  def sumCubes(a: Int, b: Int): Double =
    sum(x => x * x * x, a, b)

  def sumRec(a: Int, b: Int): Double =
    sum(x => 1.0 / x, a, b)

  sumInts(0, 3)
  
  def addNormal(a: Int, b: Int) = a +b
  def addOne(a:Int) = (b:Int) => a+b
  
  def curry(a:Int)(b:Int) = a + b
  
  def myRepeat(x:Int)(block : => Unit) : Unit = {
    if(x > 0)
      block
     myRepeat(x-1)(block)
  }
}