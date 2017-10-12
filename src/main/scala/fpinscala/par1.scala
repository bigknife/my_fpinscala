package fpinscala

trait Par[+A] {
  def run: A
}

object Par {
  def unit[A](a: => A): Par[A] = new Par[A] {
    def run: A = a
  }
  def get[A](pa: Par[A]): A = pa.run

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = {
    val a = get(pa)
    val b = get(pb)
    unit(f(a, b))
  }
}

object ParMain extends App{
  def sum(as: IndexedSeq[Int]): Par[Int] =
    if (as.size <= 1) Par.unit(as.headOption getOrElse 0)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }

  println(sum(IndexedSeq(1,2,3,4,5)).run)
}
