package fpinscala.state

sealed trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def apply(seed: Long): RNG = SimpleRNG(seed)

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n       = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }
}

final case class State[S, +A](run: S => (A, S)) { self =>
  import State._

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

}

object State {

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  type Rand[A] = State[RNG, A]

  object Rand {
    def int: Rand[Int] = State(_.nextInt)

    def nonNegativeInt: Rand[Int] = int.map(x => if(x < 0) -(x + 1) else x)
  }

}

object StateApp extends App {
  import State._, Rand._

  val seed = RNG(1)

  val str = Array(
    "a", "e", "i", "o", "u"
  )

  val randInt = int
  val randDouble = int.map(_.toDouble.abs / Double.MaxValue)
  val randStr = int.map(x => str(x.abs % str.length))

  val r = for {
    a <- randInt
    b <- randDouble
    c <- randStr
    d <- randStr
    e <- randInt
  } yield (a, b, c, d, e)

  println(r.run(seed)._1)

}
