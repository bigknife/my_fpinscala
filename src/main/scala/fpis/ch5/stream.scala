package fpis.ch5

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty
}

object Stream {
  def empty[A]: Stream[A] = new Stream[A] {
    def uncons: Option[(A, Stream[A])] = None
    override def toString: String      = "empty"
  }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons: Option[(A, Stream[A])] = Some(hd, tl)
      override def toString: String           = s"$hd #:: <tl>"
    }

  def zip[A, B](sa: Stream[A], sb: Stream[B]): Stream[(A, B)] = {
    for {
      a <- sa
      b <- sb
    } yield (a, b)
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  implicit final class StreamOp[+A](stream: Stream[A]) {
    def #::[B >: A](a: B): Stream[B] = cons(a, stream)

    def zip[B](sb: Stream[B]): Stream[(A, B)] = Stream.zip(stream, sb)

    def toList: List[A] = {
      def go(acc: List[A], rest: Stream[A]): List[A] =
        rest.uncons match {
          case None             => acc
          case Some((hd, tail)) => go(acc :+ hd, tail)
        }
      go(Nil, stream)
    }

    def headOption: Option[A] = stream.uncons match {
      case None          => None
      case Some((hd, _)) => Some(hd)
    }

    def tail: Stream[A] = stream.uncons match {
      case None            => empty[A]
      case Some((_, tail)) => tail
    }

    def take(n: Int): Stream[A] = stream.uncons match {
      case None                       => empty
      case Some((hd, tail)) if n <= 0 => empty
      case Some((hd, tail)) if n <= 1 => cons(hd, empty)
      case Some((hd, tail))           => cons(hd, tail.take(n - 1))
    }

    def takeWhile(p: A => Boolean): Stream[A] = stream.uncons match {
      case None                      => empty
      case Some((hd, tail)) if p(hd) => cons(hd, tail.takeWhile(p))
      case Some((hd, tail))          => tail.takeWhile(p)
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      stream.uncons match {
        case Some((h, t)) => f(h, t.foldRight(z)(f))
        case None         => z
      }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, acc) => p(a) || acc)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, acc) => p(a) && acc)

    def takeWhileOverFoldRight(p: A => Boolean): Stream[A] =
      foldRight(empty[A]) { (a, acc) =>
        if (p(a)) cons(a, acc)
        else acc
      }

    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B]) { (a, acc) =>
        cons(f(a), acc)
      }

    def product[B](s: Stream[B]): Stream[(A, B)] =
      unfold((stream, s))(ss =>
        (ss._1.headOption, ss._2.headOption) match {
          case (Some(h1), Some(h2)) => Some(((h1, h2), (ss._1.tail, ss._2.tail)))
          case _                    => None
      })

    def map2[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
      product(s).map(x => f(x._1, x._2))

    def mapOverUnfold[B](f: A => B): Stream[B] =
      unfold(stream) { s =>
        s.headOption match {
          case None    => None
          case Some(h) => Some(f(h), s.tail)
        }
      }

    def filter(p: A => Boolean): Stream[A] =
      foldRight(empty[A]) { (a, acc) =>
        if (p(a)) cons(a, acc)
        else acc
      }

    def find(p: A => Boolean): Option[A] =
      filter(p).headOption

    def append[B >: A](tail: Stream[B]): Stream[B] =
      foldRight(tail)(cons(_, _))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B]) { (a, acc) =>
        f(a).append(acc)
      }

    def startsWith[B >: A](sub: Stream[B]): Boolean = (stream.headOption, sub.headOption) match {
      case (_, None)            => true
      case (None, Some(_))      => false
      case (Some(h1), Some(h2)) => (h1 == h2) && stream.tail.startsWith(sub.tail)
    }

    def tails: Stream[Stream[A]] = headOption match {
      case None    => empty[Stream[A]]
      case Some(_) => cons(stream, stream.tail.tails)
    }

    def scanRight[B](z: B)(f: (A, B) => B): Stream[B] = {
      tails.foldRight(cons(z, empty[B])) { (as, acc) =>
        // as: Stream[A]
        val asb = as.foldRight(z) { (a, b) =>
          f(a, b)
        }
        cons(asb, acc)
      }
    }

  }

  //////////////////////////////////////////////////////////////
  // some interesting ctor
  /////////////////////////////////////////////////////////////

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None         => empty[A]
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  def from(n: Int): Stream[Int] =
    unfold[Int, Int](n)(x => Some((x, x + 1)))

  def constantOverUnfold[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))

  //// pcn, previous, current, next
  def fibs: Stream[Long] = unfold((0L, 0L)) {
    case (0L, 0L) => Some(0L, (1L, 1L))
    case (c, n)   => Some(n, (n, c + n))
  }

}
