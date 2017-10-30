package fpis.ch5

object strict {
  def if2[A](b: Boolean)(_true: => A, _false: => A): A =
    if (b) _true else _false

  def pair[A](a: => A): (A, A) = (a, a)
  def pair2[A](a: => A): (A, A) = {
    lazy val b = a
    (b, b)
  }
}
