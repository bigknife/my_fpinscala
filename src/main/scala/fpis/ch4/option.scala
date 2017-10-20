package fpis.ch4

sealed trait Maybe[+A]

object Maybe {
  case object None extends Maybe[Nothing]

  case class Some[A](get: A) extends Maybe[A]

  implicit final  class MaybeOps[A](maybe: Maybe[A]) {
    def map[B](f: A => B): Maybe[B] = maybe match {
      case None => None
      case Some(x) => Some(f(x))
    }

    def flatMap[B](f: A => Maybe[B]): Maybe[B] = maybe match {
      case None => None
      case Some(x) => f(x)
    }

    def getOrElse(default: => A): A = maybe match {
      case None => default
      case Some(x) => x
    }

    def orElse(other: => Maybe[A]): Maybe[A] = maybe match {
      case None => other
      case x => x
    }

    def filter(f: A => Boolean): Maybe[A] = maybe match {
      case Some(x) if f(x) => Some(x)
      case _ => None
    }
  }
  //implicit def toMaybeOps[A](maybe: Maybe[A]): MaybeOps[A] = MaybeOps[A](maybe)

  implicit final class Syntax[A](a: A) {
    def none: Maybe[A] = None
    def some: Maybe[A] = Some(a)
  }

  //implicit def toSyntax[A](a: A): Syntax[A] = Syntax(a)

}
