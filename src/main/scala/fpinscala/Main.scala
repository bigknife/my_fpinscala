package fpinscala


sealed trait Stream[+A] {
  import Stream._

  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = {
    if (isEmpty) List.empty[A]
    else {
      val (head, tail) = uncons.get

      def go(acc: List[A], hd: A, tl: Stream[A]): List[A] = {
        val now = acc :+ hd
        tl.uncons match {
          case Some((hd1, tl1)) => go(now, hd1, tl1)
          case None => now
        }
      }
      go(List.empty[A], head, tail)
    }
  }

  def take(n: Int): Stream[A] =
    if (n <= 0) empty[A]
    else
      uncons match {
        case None => empty[A]
        case Some((head, tail)) =>
          cons(head, tail.take(n -1))
      }

  def takeWhile(p: A => Boolean): Stream[A] =
    if (isEmpty) empty[A]
    else {
      val (head, tail) = uncons.get
      if (p(head)) cons(head, tail.takeWhile(p))
      else tail.takeWhile(p)
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((head, tail)) =>
        //println(s"foldRight $head")
        f(head, tail.foldRight(z)(f))
      case None => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false) {(next, acc) =>
      p(next) || acc
    }

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B]){(next, acc) =>
      cons(f(next), acc)
    }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A]){(next, acc) =>
      if (f(next)) cons(next, acc)
      else acc
    }


}

object Stream {
  def empty[A] = new Stream[A] {
    def uncons: Option[(A, Stream[A])] = None
  }

  def cons[A](head: A, tail: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((head, tail))
    }

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def fibs: Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some((a, s)) => cons(a, unfold(s)(f))
  }
    

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty[A]
    else cons(as.head, apply(as.tail:_*))
}

object Main extends App {
  println("hello,world")

  val s = Stream[Int]((1 to 1000000): _*)
  val s1 = s.take(10000)
  val s2 = s.takeWhile(_ % 2 == 0)

 

  println(s.toList)
  println(s1.toList)
  println(s2.toList)

}
