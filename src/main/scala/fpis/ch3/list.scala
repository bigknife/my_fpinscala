package fpis.ch3

object list {
  sealed trait List[+A] {
    def ::[B >: A](b: B): List[B] = this match {
      case Nil ⇒ fpis.ch3.list.::(b, Nil)
      case x @ fpis.ch3.list.::(_, _) ⇒ fpis.ch3.list.::(b, x)
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

  def sum(ints: List[Int]): Int = ints match {
    case Nil ⇒ 0
    case head :: tail ⇒ head + sum(tail)
  }


  println(list)
}