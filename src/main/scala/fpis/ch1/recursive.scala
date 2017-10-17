package fpis.ch1

object recursive {
  def factorial(n: Int): Int =
    if (n == 0) 1
    else n * factorial(n - 1)

  def factorial1(n: Int): Int = {
    @annotation.tailrec
    def go(acc: Int, n: Int): Int =
      if(n == 0) acc
      else go(acc * n, n -1)

    go(1, n)
  }
}