package ch8

object properties {
  def sum(as: List[Int]): Int = as.foldLeft(0)((acc, a) => acc + a)

  def sumProperties: Boolean = {
    val list     = List(1, 2, 3, 4, 5)
    val reversed = list.reverse
    sum(list) == sum(reversed)
  }
}
