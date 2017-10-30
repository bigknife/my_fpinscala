package fpis.ch8
import fpis.ch6.state1._
import fpis.ch5.Stream

trait Prop {
}

object Gen {
  type Domain[+A] = Stream[Option[A]]

  object Domain {
    def bounded[A](a: Stream[A]): Domain[A] = a.map(Some(_))
    def unbounded: Domain[Nothing] = Stream(None)

    def map[A, B](domain: Domain[A])(f: A => B): Domain[B] = {
      val f1: Option[A] => Option[B] = _.map(f)
      domain.map(f1)
    }

    def map2[A, B, C](d1: Domain[A], d2: Domain[B])(f: (A, B) => C): Domain[C] = {
      val f1: (Option[A], Option[B]) => Option[C] = (oa, ob) => (oa, ob) match {
        case (Some(a),Some(b)) => Some(f(a,b))
        case _ => None: Option[C]
      }
      d1.map2(d2)(f1)
    }

    def flatMap[A, B](d: Domain[A])(f: A => Domain[B]): Domain[B] = {
      val f1: Option[A] => Stream[Option[B]] = x => x match {
        case None => Stream(None)
        case Some(y) => f(y)
      }
      d.flatMap(f1)
    }
  }


  case class Gen[+A](sample: State[RNG, A], exhaustive: Domain[A]) { self =>
    def map[B](f: A => B): Gen[B] = self.copy(sample = sample.map(f), exhaustive = Domain.map(exhaustive)(f))

    def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = self.copy(
      sample = sample.map2(g.sample)(f),
      exhaustive = Domain.map2(self.exhaustive, g.exhaustive)(f)
    )

    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(
      sample = sample.flatMap(x => f(x).sample),
      exhaustive = Domain.flatMap(self.exhaustive)(x => f(x).exhaustive)
    )
  }



  def rng(seed: Long): RNG = SimpleRNG(seed)

  def unit[A](a: => A): Gen[A] = Gen(State.unit[RNG, A](a), Domain.unbounded)



  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(
    State[RNG,Int](_.nextInt).map(x => start + (x % (stopExclusive - start)).abs),
    Domain.unbounded
  )

  def boolean: Gen[Boolean] = choose(Int.MinValue + 1, Int.MaxValue).map(_ % 2 == 0)

  def choosePair(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    choose(start, stopExclusive).map2(choose(start, stopExclusive))((_, _))

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] = choose(from, to).map(x => (x, x))


  def listOfN_1(size: Gen[Int]): Gen[List[Int]] = {
    // assume from and to
    val from = Int.MinValue + 1
    val to = Int.MaxValue
    size.flatMap(i => {
      def go(n: Int, acc: Gen[List[Int]]): Gen[List[Int]] = {
        if (n == 0) acc
        else {
          val nextAcc: Gen[List[Int]] = for {
            as <- acc
            a <- choose(from, to)
          } yield as :+ a

          go(n -1, nextAcc)
        }
      }
      go(i, unit(List[Int]()))
    })
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    def go(n: Int, gs: Gen[List[A]]): Gen[List[A]] = if (n == 0) gs else {
      val ngs = for {
        a <- g
        b <- gs
      } yield b :+ a

      go(n -1, ngs)
    }
    go(n, unit(List[A]()))
  }


  /*
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(rng => {
      val (i, rng1) = rng.nextInt
      (start + (i  % (stopExclusive - start)).abs, rng1) 
    })
   */
  

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???
  //def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???
}
