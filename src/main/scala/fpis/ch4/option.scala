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

    def map2[B, C](maybeB: Maybe[B])(f: (A, B) => C): Maybe[C] = (maybe, maybeB) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(a), Some(b)) => Some(f(a,b))
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

object MaybeApp extends App {
  import Maybe._

  def lift[A, B](f: A => B): Maybe[A] => Maybe[B] = _ map f

  import java.util.regex._
  def pattern(s: String): Maybe[Pattern] = try {
    Pattern.compile(s).some
  } catch {
    case e: Throwable => None
  }

  def matcher(pat: String): Maybe[String => Boolean] =
    pattern(pat).map(p => (s: String) => p.matcher(s).matches)

  def matched(pat: String, s: String): Maybe[Boolean] =
    for{
      f <- matcher(pat)
    } yield f(s)

  def bothMatched(pat1: String, pat2: String, s: String): Maybe[Boolean] =
    for {
      f1 <- matcher(pat1)
      f2 <- matcher(pat2)
    } yield (f1(s) && f2(s))

  def bothMatched_1(pat1: String, pat2: String, s: String): Maybe[Boolean] =
    matcher(pat1).map2(matcher(pat2))(_(s) && _(s))

  def sequence[A](as: List[Maybe[A]]): Maybe[List[A]] = {
    as.foldLeft(List[A]().some){(acc, a) =>
      for {
        x <- acc
        y <- a
      } yield  x :+ y
    }
  }

  def traverse[A, B](as: List[A])(f: A => Maybe[B]): Maybe[List[B]] = as match {
    case Nil => Some(Nil)
    case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
  }

}
