package fpis.ch4

sealed trait Either[+E, +A]

object Either {
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  
}
