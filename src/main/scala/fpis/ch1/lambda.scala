package fpis.ch1

object lambda extends App {
  val formatResult: (String, Int, Int ⇒ Int) ⇒ String = (s, i, f) ⇒ s"The $s of $i is ${f(i)}"

  val addOne: Int ⇒ Int = x ⇒ x + 1
  def addOne1(i: Int): Int = i + 1

  println(formatResult("add one", 1, addOne))
  println(formatResult("add one", 1, addOne1))
  println(formatResult("add one", 1, x ⇒ x + 1))
  println(formatResult("add one", 1, _ + 1))
}