package fpinscala.datastructures

sealed trait List[+A] { self =>
}

case object Nil extends List[Nothing]

case class Cons[A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](a: A*): List[A] =
    if(a.isEmpty) Nil
    else Cons(a.head, apply(a.tail: _*))


  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum(ints: List[Int]): Int =
    foldRight(ints, 0)(_ + _)

  def product(l: List[Double]): Double =
    foldRight(l, 1.0)(_ * _)

  def length[A](l: List[A]): Int =
    foldRight(l, 0) {(_, acc) => acc + 1}

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight[A, List[B]](l, Nil) {(n, acc) =>
      Cons(f(n), acc)
    }
}

object ListApp extends App {
  import List._
  val l = List(1,2,3,4,5,6)
  println(" length = " + length(l))
  println(" sum = " + sum(l))

  println(map(l)(_ * 1.0))
}