package fpis.ch5

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty
}

object Stream {
  def empty[A]: Stream[A] = new Stream[A] {
    def uncons: Option[(A, Stream[A])] = None
  }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons: Option[(A, Stream[A])] = Some(hd, tl)
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  implicit final class StreamOp[+A](stream: Stream[A]) {
    def #::[B >: A](a: B): Stream[B] = cons(a, stream)

    def toList: List[A] = {
      def go(acc: List[A], rest: Stream[A]): List[A] =
        rest.uncons match {
          case None => acc
          case Some((hd, tail)) => go(acc :+ hd, tail)
        }
      go(Nil, stream)
    }

    def take(n: Int): Stream[A] = stream.uncons match {
      case None => empty
      case Some((hd, tail)) if n <= 0 => empty
      case Some((hd, tail)) if n <= 1 => cons(hd, empty)
      case Some((hd, tail)) => cons(hd, tail.take(n-1))
    }

    def takeWhile(p: A => Boolean): Stream[A] = stream.uncons match {
      case None => empty
      case Some((hd, tail)) if p(hd) => cons(hd, tail.takeWhile(p))
      case Some((hd, tail))  => tail.takeWhile(p)
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      stream.uncons match {
        case Some((h, t)) => f(h, t.foldRight(z)(f))
        case None => z
      }
  }
}
