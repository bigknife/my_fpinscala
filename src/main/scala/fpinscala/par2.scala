package fpinscala

package par2

import java.util.concurrent._
import language.implicitConversions

object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): A = a(s).get
  def unit[A](a: A): Par[A] = (s: ExecutorService) => UnitFuture(a)

  // unit future
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }


  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = (s: ExecutorService) => {
    val a = pa(s)
    val b = pb(s)
    UnitFuture(f(a.get, b.get))
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  def fork[A](a: => Par[A]): Par[A] = (s: ExecutorService) => {
    s.submit(new Callable[A]() {
      def call: A = a(s).get
    })
  }

  def async[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => async(f(a))

  def sortPar(l: Par[List[Int]]): Par[List[Int]] = map(l)(_.sorted)
  def sortPar1(l: Par[List[Int]]): Par[List[Int]] = (s: ExecutorService) => {
    val pf = l(s)
    UnitFuture(pf.get.sorted)
  }

  def product[A, B](fa: Par[A], fb: Par[B]): Par[(A, B)] = (s: ExecutorService) => {
    val va = fa(s).get
    val vb = fb(s).get
    UnitFuture((va, vb))
  }

  def map_1[A, B](fa: Par[A])(f: A => B): Par[B] = (s: ExecutorService) => {
    val va = fa(s).get
    UnitFuture(f(va))
  }

  def map2_1[A, B, C](fa: Par[A], fb: Par[B])(f: (A, B) => C): Par[C] =
    map(product(fa, fb))(x => f(x._1, x._2))

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = (s: ExecutorService) => {
    l.foldLeft(UnitFuture(List.empty[B])){(acc, n) =>
      val vb = acc.get :+ f(n)
      UnitFuture(vb)
    }
  }
}

object ParApp extends App {
  import Par._
  println(s"mainThread: ${Thread.currentThread()}")
  val f: Int => Int = x => { println(s"currentThread: ${Thread.currentThread()}");x + 1}
  val parF = asyncF(f)

  val s: ExecutorService = Executors.newFixedThreadPool(2)
  val r = run(s)(parF(1))
  println(s"ok: $r")

  println(parMap(List(1,2,3,4,5,6,7,8,9,10))(f)(s))

  s.shutdown()
}
