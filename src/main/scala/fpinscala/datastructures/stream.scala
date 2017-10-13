package fpinscala.datastructures


trait Stream[+A] {
  import Stream._

  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = {
    def go(acc: List[A], stream: Stream[A]): List[A] = stream.uncons match {
      case None => acc
      case Some((head, tail)) => go(Cons(head,acc), tail)
    }

    go(Nil, this.reverse)
  }

  def toScalaList: scala.List[A] = {
    def go(acc: scala.List[A], stream: Stream[A]): scala.List[A] = stream.uncons match {
      case None => acc
      case Some((head, tail)) => go(head +: acc, tail)
    }

    go(scala.List.empty[A], this)
  }

  def take(n: Int): Stream[A] = {
    def go(acc: Stream[A], restStream: Stream[A], n1: Int): Stream[A] = restStream.uncons match {
      case None => acc
      case Some((h, t)) =>
        if (n1 <= 0 ) acc
        else go(cons(h, acc), t, n1 -1)
    }

    go(empty[A], this, n).reverse
  }

  def reverse: Stream[A] = {
    def go(acc: Stream[A], restStream: Stream[A]): Stream[A] = restStream.uncons match {
      case None => acc
      case Some((h, t)) =>
        go(cons(h, acc), t)
    }
    go(empty, this)
  }

  def map[B](f: A => B): Stream[B] = uncons match {
    case None => empty[B]
    case Some((h, t)) => cons(f(h), t.map(f))
  }
}

object Stream {
  def empty[A]: Stream[A] = new Stream[A] {
    override def uncons: Option[(A, Stream[A])] = None

    override def toString: String = "Stream()"
  }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      override def uncons: Option[(A, Stream[A])] = Some(hd, tl)

      override def toString: String = s"Stream($hd, function<tail>)"
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty[A]
    else cons(as.head, apply(as.tail: _*))
}

object StreamApp extends App {
  import Stream._

  val stream = Stream(1 to 100000: _*)
  stream.map(_.toString + ", aho").take(100000).toScalaList foreach println
}