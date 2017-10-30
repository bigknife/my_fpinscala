package fpis.ch6.rng2

import fpis.ch6._

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
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int]         = _.nextInt
  def unit[A](a: A): Rand[A] = (a, _)

  def positiveMax(n: Int): Rand[Int] = int.map(_.abs).map(_ % n)

  def double: Rand[Double] = int.map(_.toDouble.abs / Double.MaxValue)

  def positiveInt: Rand[Int] = int.map {
    case x if x != Int.MinValue => x.abs
    case x                      => (x - 1).abs
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]())) { (ra, acc) =>
      ra.map2(acc)(_ +: _)
    }

  implicit final class RandOps[+A](rand: Rand[A]) {
    def map[B](f: A => B): Rand[B] = rng => {
      val (a, rng1) = rand(rng)
      (f(a), rng1)
    }

    def map2[B, C](rand2: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
      val (a, rng1) = rand(rng)
      val (b, rng2) = rand2(rng1)
      (f(a, b), rng2)
    }

    def flatMap[B](f: A => Rand[B]): Rand[B] = rng => {
      val (a, rng1) = rand(rng)
      f(a)(rng1)
    }

    def flatMapOverMap2[B](f: A => Rand[B]): Rand[B] = rng => {
      /*
      val rand2: Rand[Unit] = unit[Unit](())
      val rand3: Rand[Rand[B]] = map2(rand2)((a, _) => f(a))
      val (rand4, rng1) = rand3(rng)
      rand4(rng1)
       */
      val (rand, rng1) = map2(unit[Unit](()))((a, _) => f(a))(rng)
      rand(rng1)
    }

    def mapOverFlatMap[B](f: A => B): Rand[B] =
      flatMap(a => unit(f(a)))

    def map2OverFlatMap[B, C](rand2: Rand[B])(f: (A, B) => C): Rand[C] =
      flatMap(a => rand2.flatMap(b => unit(f(a, b))))

    def map2OverFlatMap_1[B, C](rand2: Rand[B])(f: (A, B) => C): Rand[C] =
      for {
        a <- rand
        b <- rand2
      } yield f(a, b)
  }
}
