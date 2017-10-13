package fpinscala

package testing




trait Gen[+A]

trait Prop {
  import Prop._

  def check: Either[FailedCase, SuccessCount]

  def &&(p: Prop): Prop
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
}

object Gen {

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

}


object TestingApp extends App {

}