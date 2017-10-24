package fpis.ch6.state1

import fpis.ch6._

/*
object state {
  type State[S, +A] = S => (A, S)
}
*/

final case class State[S, +A](run: S => (A, S)) {
  import State._

  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      (f(a), s1)
    })

  def map2[B, C](state1: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s1) = run(s)
      val (b, s2) = state1.run(s1)
      (f(a, b), s2)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =State(s => {
    /*
    val _u = unit[S, Unit](())
    val m: State[S, State[S,B]] = map2(_u)((a, _) => f(a))
    val (sb, s1) = m.run(s)
    sb.run(s1)
     */
    val (sb, s1) = map2(unit[S, Unit](()))((a, _) => f(a)).run(s)
    sb.run(s1)
  })
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
}

sealed trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val seed2 = lcg(seed)
    ((seed2 >>> 16).asInstanceOf[Int], SimpleRNG(seed2))
  }
}


object rand {
  type Rand[+A] = State[RNG, A]

  def unit[A](a: A): Rand[A] = State.unit[RNG,A](a)

  val int: Rand[Int] = State(_.nextInt)
  val double: Rand[Double] = int.map(_.toDouble.abs / Double.MaxValue)

  val doubleInt: Rand[(Double, Int)] = for {
    d <- double
    i <- int
  } yield ((d, i))

  val intDouble: Rand[(Int, Double)] = doubleInt.map(x => (x._2, x._1))
}
