package fpis.ch8

import fpis.ch6.state1._

trait Gen[+A]

trait Prop { self =>
  import Prop._

  /*
  def check: Boolean

  def &&(p: Prop): Prop = new Prop {
    def check: Boolean = self.check && p.check
  }
   */

  def check: Either[FailedCase, SuccessCount]

}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
}



object gen {
  type Gen[A] = State[RNG, A]

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(rng => {
      val (i, rng1) = rng.nextInt
      ((i  % (stopExclusive - start)).abs, rng1) 
    })

  

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???
}
