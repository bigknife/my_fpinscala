package fpis.ch8

import scala.language.higherKinds

import fpis.ch6.state1.RNG
import fpis.ch5.Stream

case class TestCases(total: Int)
case class FailedCase(message: String)
case class SuccessCount(value: Int)

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  val isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  val isFalsified: Boolean = true
}

case class Prop(run: (TestCases, RNG) => Result)

object Prop {

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(as)(rng)
      .zip(Stream.from(0))
      .take(n.total)
      .map {
        case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(FailedCase(a.toString), SuccessCount(i))
          } catch { case e: Exception => Falsified(FailedCase(buildMsg(a, e)), SuccessCount(i)) }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test cases: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}
