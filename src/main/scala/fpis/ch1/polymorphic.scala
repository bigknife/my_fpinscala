package fpis.ch1

object polymorphic extends App {
  def binarySearch[A](as: Array[A], key: A, gt: (A, A) ⇒ Boolean): Int = {

    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid -1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = gt(a, key)
        if (!greater && ! gt(key, a)) mid2
        else if (greater) go(low, mid2, mid2 -1)
        else go(mid2 + 1, mid2, high)
      }
    }

    go(0, 0, as.length - 1)
  }

  val pos = binarySearch("what the fuck".toCharArray, 'e', (c1: Char, c2: Char) ⇒ (c1 - c2) > 0)
  println(s"pos is $pos")
}