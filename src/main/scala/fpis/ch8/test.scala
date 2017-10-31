package fpis.ch8
import fpis.ch6.state1._
import fpis.ch5.Stream

import scala.language.higherKinds

case class Gen[A](sample: State[RNG, A])

object Gen {
  import ops._

  def rng(seed: Long): RNG = SimpleRNG(seed)

  def choose(start: Int, stopExeclusive: Int): Gen[Int] = {
    val intState = State[RNG, Int](_.nextInt)
    val mapped   = intState.map((x: Int) => start + (x.abs % (stopExeclusive - start)))
    Gen(mapped)
  }

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit[RNG, A](a))

  def boolean: Gen[Boolean] = choose(Int.MinValue, Int.MaxValue).map(_ % 2 == 0)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    def go(n: Int, g: Gen[A], acc: Gen[List[A]]): Gen[List[A]] = {
      if (n == 0) acc
      else {
        val next = for {
          a  <- g
          as <- acc
        } yield as :+ a
        go(n - 1, g, next)
      }

    }
    go(n, g, unit[List[A]](List[A]()))
  }

  object ops {
    implicit def genOps[A](gen: Gen[A]): GenOps[A] = new GenOps(gen)
  }

}

private[fpis] final class GenOps[A](gen: Gen[A]) {
  def runValue(rng: RNG): A = gen.sample.run(rng)._1

  def map[B](f: A => B): Gen[B] = Gen(
    gen.sample.map(f)
  )

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(
    State(rng => {
      val (a, rng1) = gen.sample.run(rng)
      val genb      = f(a)
      genb.sample.run(rng1)
    })
  )
}
