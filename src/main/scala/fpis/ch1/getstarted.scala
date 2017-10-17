package fpis.ch1

object GetStarted extends App {
  private def formatResult(name: String, n: Int, f: Int ⇒ Int) = {
    val msg = "The %s of %d is %d."
    val v   = f(n)
    msg.format(name, n, v)
  }


  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  def fib(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else {
      @annotation.tailrec
      def go(i: Int, v: Int, p1: Int, p2: Int, max: Int): Int = {
        i match {
          case 0             ⇒ go(1, 0, 0, 0, n)
          case 1             ⇒ go(2, 1, 1, 0, n)
          case x if x >= max ⇒ p1 + p2
          case x             ⇒ go(x + 1, p1 + p2, p1 + p2, v, max)
        }
      }
      go(0, 0, 0, 0, n)
    }
  }

  println(formatResult("absolute value", -42, abs))
  println(formatResult("factorial", 3, factorial))
  println(formatResult("fib", 20, fib))

  def fibLaw(i: Int): Boolean = {
    val p1 = fib(i)
    val p2 = fib(i + 1)
    val p3 = fib(i + 2)
    p3 == p1 + p2
  }

  println(fibLaw(20))

  case class Fibonacci(
      idx: Int,
      value: Long,
      previous: Option[Fibonacci]
  ) {
    def next: Fibonacci = Fibonacci(idx + 1, value + previous.map(_.value).getOrElse(0L), Some(this))
  }
  object Fibonacci {
    def zero: Fibonacci = Fibonacci(0, 0, None)
    def one: Fibonacci = Fibonacci(1, 1, Some(zero))

    def take(n: Int): Fibonacci =
      if (n == 0) zero
      else if (n == 1) one
      else {
        def next(current: Fibonacci): Fibonacci =
          if (n == current.idx) current
          else next(current.next)

        next(one)
      }
  }

  val fib1 = Fibonacci.take(50)
  println(fib1.value)
}
