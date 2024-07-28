package datastructures

sealed trait SimpleList[+A]
case object Nil extends SimpleList[Nothing]
case class Cons[+A](head: A, tail: SimpleList[A]) extends SimpleList[A]

object SimpleList {
  def sum(ints: SimpleList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: SimpleList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): SimpleList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x: Int = SimpleList(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case _ => 101
  }

  def tail[A](ls: SimpleList[A]): SimpleList[A] = ls match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](head: A, sl:SimpleList[A]): SimpleList[A] = sl match {
    case Nil => Cons(head, Nil)
    case Cons(_, t) => Cons(head, t)
  }

  @annotation.tailrec
  def drop[A](l: SimpleList[A], n: Int): SimpleList[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  @annotation.tailrec
  def dropWhile[A](l: SimpleList[A], f: A => Boolean): SimpleList[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }
}