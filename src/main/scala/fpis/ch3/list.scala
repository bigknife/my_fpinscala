package fpis.ch3

object list {
  sealed trait List[+A] {
    def ::[B >: A](b: B): List[B] = this match {
      case Nil ⇒ fpis.ch3.list.::(b, Nil)
      case x @ fpis.ch3.list.::(_, _) ⇒ fpis.ch3.list.::(b, x)
    }

    def take(n: Int): List[A] =
      if (n <= 0) Nil
      else {
        def go[B >: A](n: Int, acc: List[B], current: List[B]): List[B] =
          if (n <= 0) acc
          else current match {
            case Nil => acc
            case fpis.ch3.list.::(head, tail) => go(n -1, head :: acc, tail)
          }

        go(n, List[A](), this)
      }
  }

  case object Nil extends List[Nothing]

  case class ::[A](head: A, tail: List[A]) extends List[A] {
    override def toString: String = {
      head.toString + " :: " + tail.toString
    }
  }

  object List {
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else ::(as.head, apply(as.tail: _*))
  }

}

import list._
object listApp extends App {
  val list: List[String] = "Hello" :: "world" :: Nil

  // pattern matching
  def sum(ints: List[Int]): Int = ints match {
    case Nil ⇒ 0
    case head :: tail ⇒ head + sum(tail)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil ⇒ 1.0
    case 0.0 :: _ ⇒ 0
    case head :: tail ⇒ head * product(tail)
  }

  // data sharing
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case head :: tail => tail
  }

  @annotation.tailrec
  def drop[A](as: List[A], n: Int): List[A] = as match {
    case Nil => Nil
    case head :: tail if n <= 1 => tail
    case head :: tail => drop(tail, n-1)
  }

  def dropWhile[A](as: List[A])(p: A => Boolean): List[A] = {
    @annotation.tailrec
    def go[A](acc: List[A], current: List[A], p: A => Boolean): List[A] = current match {
      case Nil => acc
      case head :: tail if !p(head) => go(head :: acc, tail, p)
      case head :: tail => go(acc, tail, p)
    }
    reverse(go(Nil, as, p))
  }

  def reverse[A](as: List[A]): List[A] = {
    @annotation.tailrec
    def go[A](acc: List[A], current: List[A]): List[A] = current match {
      case Nil => acc
      case head :: tail => go(head :: acc, tail)
    }
    go(Nil, as)
  }

  def append[A](as1: List[A], as2: List[A]): List[A] = as1 match {
    case Nil => as2
    case head :: tail => head :: append(tail, as2)
  }

  def append1[A](as1: List[A], as2: List[A]): List[A] = {
    val reversedAs1 = reverse(as1)
    def go[B](acc: List[B], current: List[B]): List[B] = current match {
      case Nil => acc
      case head :: tail => go(head :: acc, tail)
    }
    go(as2, reversedAs1)
  }

  def setHead[A](as: List[A], head: A): List[A] = as match {
    case Nil => Nil
    case _ :: tail => head :: tail
  }

  def init[A](as: List[A]): List[A] = {
    def go(acc: List[A], current: List[A]): List[A] = current match {
      case Nil => acc
      case head :: Nil => acc
      case head :: tail => go(head :: acc, tail)
    }
    reverse(go(Nil, as))
  }

  // abstraction
  // recursion over list and generalizing to HOF
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case head :: tail => f(head, foldRight(tail, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case head :: tail => foldLeft(tail, f(z, head))(f)
    }
  }

  def foldRight1[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((b, a) => f(a, b))
  }

  def foldLeftOverFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)((a,g) => b => g(f(b, a)))(z)

  def sum2(l: List[Int]): Int =
    foldRight(l, 0)(_ + _)

  def product2(l: List[Int]): Int =
    foldRight(l, 1)(_ * _)

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldLeft(reverse(as), List[B]()) {(acc, a) => f(a) :: acc}

  def concat[A](ass: List[List[A]]): List[A] =
    foldRight(ass, List[A]())(append)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def reverse1[A](as: List[A]): List[A] =
    foldLeft(as, List[A]()){(acc, a) => a :: acc}

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldLeft(reverse1(as), List[A]()){(acc, a) => if (f(a)) a :: acc else acc}

  println(list)
}
