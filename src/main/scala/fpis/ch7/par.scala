package fpis.ch7

import java.util.concurrent.{ExecutorService, Future}


object Par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone: Boolean = true
    def cancel(evenIfRunning: Boolean): Boolean = false
    def isCancelled: Boolean = true
  }

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  def get[A](a: Par[A]): A = ???

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???
}

object ParApp extends App {
  def sum(as: IndexedSeq[Int]): Par[Int] =
    if (as.size <= 1) Par.unit(as.headOption getOrElse 0)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }
}
