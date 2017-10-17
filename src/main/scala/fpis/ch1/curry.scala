package fpis.ch1

object curry extends App {
  def add(i1: Int, i2: Int): Int = i1 + i2
  
  def curry[A, B, C](f: (A, B) => C): A => B => C = (a: A) => (b: B) => f(a, b)
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)
  
  def printResult(name: String, arg: Int, f: Int => Int): Unit =
    println(s"The $name of $arg is ${f(arg)}")

  printResult("add1", 10, curry(add)(1))
  printResult("add2", 10, add(2, _))
}