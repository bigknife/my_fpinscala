package fpis.ch3

sealed trait Tree[+A]

object Tree {
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def leaf[A](value: A): Tree[A] = Leaf(value)

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  final case class TreeOps[A](tree: Tree[A]) {
    def size: Int = tree match {
      case Leaf(_) => 1
      case Branch(l, r) => TreeOps(l).size + TreeOps(r).size
    }

    def maximum(gt: (A, A) => Boolean): A = tree match {
      case Leaf(v) => v
      case Branch(l, r) =>
        val maxL = TreeOps(l).maximum(gt)
        val maxR = TreeOps(r).maximum(gt)
        if (gt(maxL, maxR)) maxL else maxR
    }

    def depth: Int = tree match {
      case Leaf(_) => 1
      case Branch(l, r) => TreeOps(l).size max TreeOps(r).size
    }

    def map[B](f: A => B): Tree[B] = tree match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(TreeOps(l).map(f), TreeOps(r).map(f))
    }

    def fold[B](z: B)(f: (B, A) => B): B = tree match {
      case Leaf(v) => f(z, v)
      case Branch(l, r) =>
        val bL = TreeOps(l).fold(z)(f)
        TreeOps(r).fold(bL)(f)
    }
  }
  implicit def to[A](tree: Tree[A]): TreeOps[A] = TreeOps(tree)
}
