package fpis.ch3

sealed trait MyList[+A] {self =>
  import MyList._

  def prepend[B >: A](a: B): MyList[B] = MyCons(a, self)

  def ::[B >: A](a: B): MyList[B] = prepend(a)

  @annotation.tailrec
  final def foldLeft[B](z: B)(f: (B, A) => B): B = self match {
    case MyNil => z
    case MyCons(head, tail) =>tail.foldLeft(f(z, head))(f)
  }

  def reverse: MyList[A] = {
    foldLeft(MyList[A]())((acc, a) => a :: acc)
  }

  def append[B >: A](a: B): MyList[B] = {
    self.reverse.prepend(a).reverse
  }

  def concat[B >: A](as: MyList[B]): MyList[B] =
    self.reverse.foldLeft(as)((acc, a) => acc.::(a))

  def take(n: Int): MyList[A] = {
    def go(n: Int, acc: MyList[A], current: MyList[A]): MyList[A] =
      if (n <= 0) acc
      else current match {
        case MyNil => acc
        case MyCons(head, tail) => go(n -1, head :: acc, tail)
      }

    go(n, MyList[A](), self).reverse
  }

  def takeWhile(f: A => Boolean): List[A] =
    foldLeft(List[A]())((acc, a) => if (f(a)) a :: acc else acc).reverse

  def forall(f: A => Boolean): Boolean =
    foldLeft(true)((acc, a) => acc && f(a)) // no short-circuit

  def forallWithShortCircuit(f: A => Boolean): Boolean = self match {
    case MyNil => false
    case MyCons(head, tail) => f(head) && tail.forall(f)
  }

  def exists(f: A => Boolean): Boolean = {
    def go(acc: Boolean, current: MyList[A]): Boolean = if (acc) true else current match {
      case MyNil => acc
      case MyCons(head, tail) => go(acc || f(head), tail)
    }
    go(false, self)
  }


  def map[B](f: A => B): MyList[B] =
    self.reverse.foldLeft(MyList[B]())((acc, a) => f(a) :: acc)

  def flatMap[B](f: A => MyList[B]): MyList[B] = {
    val list: MyList[MyList[B]] = map(f)
    list.foldLeft(MyList[B]())((acc, a) => acc.concat(a))
  }
}

object MyList {
  case object MyNil extends MyList[Nothing] {
    override def toString: String = "Nil"
  }
  case class MyCons[A](head: A, tail: MyList[A]) extends MyList[A] {
    override def toString: String = {
      def go(acc: String, currentTail: MyList[A]): String = currentTail match {
        case MyNil => acc + " :: " + MyNil
        case MyCons(h, t) => go(h + " :: " + acc, t)
      }
      go(head.toString, tail) 
    }
  }

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) MyNil
    else MyCons(as.head, apply(as.tail: _*))

  def repeat[A](a: A, n: Int): MyList[A] = {
    def go(acc: MyList[A], n: Int): MyList[A] =
      if (n == 0) acc
      else go(MyCons(a, acc), n-1)

    go(MyList[A](), n)
  }

  def succeed[A](init: A)(f: A => Option[A]): MyList[A] = {
    def go(acc: MyList[A], v: A): MyList[A] = f(v) match {
      case Some(nv) => go(MyCons(v, acc), nv)
      case None => acc
    }
    go(MyList[A](), init)
  }
}
