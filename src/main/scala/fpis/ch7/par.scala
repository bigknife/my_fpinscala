package fpis.ch7

import java.util.concurrent.{ExecutorService, Future, Callable, TimeUnit}

object parTest {
  import par._

  def sum(as: IndexedSeq[Int]): Par[Int] =
    if (as.size <= 1) unit(as.headOption getOrElse 0)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(fork(sum(l)), fork(sum(r)))(_ + _)
    }
}

object par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](a: A) extends Future[A] {
    def get(): A                                        = a
    def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    def get(timeout: Long, unit: TimeUnit): A           = a
    def isCancelled(): Boolean                          = false
    def isDone(): Boolean                               = true
  }

  // primitive combinator
  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  // derived combinator
  def async[A](a: => A): Par[A] = fork(unit(a))

  /**
    * map2 is primitive, and map implemented in term to map2
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (s: ExecutorService) => {
      val c1 = f(a(s).get,b(s).get)
      UnitFuture(c1)
    }

  def map[A, B](fa: Par[A])(f: A => B): Par[B] =
    map2(fa, unit(()))((a, _) => f(a))
    */
  def fork[A](a: => Par[A]): Par[A] = (s: ExecutorService) => {
    // maybe deadlocked
    //   invoke get in a callable
    s.submit[A](new Callable[A] {
      def call(): A = a(s).get

    })
  }

  def asyncF[A, B](f: A => B): A => Par[B] = a => async(f(a))

  def product[A, B](fa: Par[A], fb: Par[B]): Par[(A, B)] = (s: ExecutorService) => {
    val af = fa(s).get
    val bf = fb(s).get
    UnitFuture((af, bf))
  }

  def map[A, B](fa: Par[A])(f: A => B): Par[B] = (s: ExecutorService) => {
    val af = fa(s).get
    UnitFuture(f(af))
  }

  def map2[A, B, C](fa: Par[A], fb: Par[B])(f: (A, B) => C): Par[C] =
    map(product(fa, fb))(ab => f(ab._1, ab._2))

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] =
    l.foldRight(unit(List[B]()))((a, acc) => map2(acc, unit(f(a)))(_ :+ _))

  def sequence_simple[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List[A]()))((h, t) => map2(h, t)(_ :: _))

  def sequence_right[A](l: List[Par[A]]): Par[List[A]] = l match {
    case Nil    => unit(Nil)
    case h :: t => map2(h, fork(sequence_right(t)))(_ :: _)
  }

  def sequence_balanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequence_balanced(l), sequence_balanced(r))(_ ++ _)
    }
  }

  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    map(sequence_balanced(l.toIndexedSeq))(_.toList)

  def parMapOverSequence[A, B](l: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = l.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val x: List[Par[List[A]]] = l.map(asyncF(a => if (f(a)) List(a) else List()))
    map(sequence(x))(_.flatten)
  }

  def map3[A, B, C, D](fa: Par[A], fb: Par[B], fc: Par[C])(f: (A, B, C) => D): Par[D] =
    map2(map2(fa, fb)((a, b) => (a, b)), fc)((ab, c) => f(ab._1, ab._2, c))

  def map4[A, B, C, D, E](fa: Par[A], fb: Par[B], fc: Par[C], fd: Par[D])(
      f: (A, B, C, D) => E): Par[E] =
    map2(map3(fa, fb, fc)((a, b, c) => (a, b, c)), fd)((abc, d) => f(abc._1, abc._2, abc._3, d))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
}
