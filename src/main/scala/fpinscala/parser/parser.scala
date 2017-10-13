package fpinscala.parser

import fpinscala.state.{RNG, State}

trait Prop {
  import Prop._

  def check: Either[FailedCase, SuccessCount]
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

object Gen {
  type Gen[A] = State[RNG, A]

  //exhaustively generate
  //type Gen[+A] = (State[RNG, A], List[A])

  def choose(start: Int, stopExclusive: Int): Gen[Int] = State.Rand.nonNegativeInt.map(x => start + x % (stopExclusive - start))
}

object GenApp extends App{
  import Gen._
  val seed = RNG(1)

  println(choose(1, 100).run(seed)._1)
}