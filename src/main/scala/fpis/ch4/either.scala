package fpis.ch4

sealed trait Either[+E, +A]

object Either {
  case class Left[+E](value: E)  extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  implicit final class EitherOps[+E, +A](either: Either[E, A]) {
    def map[B](f: A => B): Either[E, B] = either match {
      case Left(value)  => Left(value)
      case Right(value) => Right(f(value))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = either match {
      case Left(value)  => Left(value)
      case Right(value) => f(value)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = either match {
      case Left(_) => b
      case x       => x
    }

    def map2[EE >: E, B, C](eitherB: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      (either, eitherB) match {
        case (Right(a), Right(b)) => Right(f(a, b))
        case (Left(a), _)         => Left(a)
        case (_, Left(b))         => Left(b)
      }
  }

  implicit final class EitherSyntax[A](a: A) {
    def left[E](e: E): Either[E, A] = Left(e)
    def right[E]: Either[E, A]      = Right(a)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil    => Right(Nil)
    case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
  }

  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
    as.foldLeft(List[A]().right[E]) { (acc, a) =>
      for {
        x <- acc
        y <- a
      } yield x :+ y
    }

}
