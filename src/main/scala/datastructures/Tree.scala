package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def maximum(t :Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t : Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B, g: (B,B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f, g), fold(r)(f, g))
  }

  def size2[A](t: Tree[A]): Int = fold[A, Int](t)(_ => 1, _ + _ + 1)

  def maximum2(t :Tree[Int]): Int = fold[Int, Int](t)(x => x, _ max _)

  def depth2[A](t : Tree[A]): Int = fold[A, Int](t)(_ => 0, _ max _ + 1)

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](t)(x => Leaf(f(x)), Branch(_, _))
}