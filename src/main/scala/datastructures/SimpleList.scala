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

  def init[A](l: SimpleList[A]): SimpleList[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: SimpleList[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  val y: SimpleList[Int] = SimpleList.foldRight(SimpleList(1, 2, 3), Nil: SimpleList[Int])(Cons(_, _) )

  def length[A](as: SimpleList[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A, B](as: SimpleList[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum2(ints: SimpleList[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product2(ds: SimpleList[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def length2[A](as: SimpleList[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  def reverse[A](as: SimpleList[A]): SimpleList[A] = foldLeft(as, Nil: SimpleList[A])((acc, h) => Cons(h, acc))

  def append[A](a1: SimpleList[A], a2: SimpleList[A]): SimpleList[A] = foldRight(a1, a2)(Cons(_, _))

  def concat[A](ls: SimpleList[SimpleList[A]]): SimpleList[A] = foldRight(ls, Nil: SimpleList[A])(append)

  def plus1(ls: SimpleList[Int]): SimpleList[Int] = foldRight(ls, Nil: SimpleList[Int])((h, t) => Cons(h + 1, t))

  def doubleToString(ls: SimpleList[Double]): SimpleList[String] = foldRight(ls, Nil: SimpleList[String])((h, t) => Cons(h.toString, t))

  def map[A, B](as: SimpleList[A])(f: A => B): SimpleList[B] = foldRight(as, Nil: SimpleList[B])((h, t) => Cons(f(h), t))

  def filter[A](as: SimpleList[A])(f: A => Boolean): SimpleList[A] = as match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
    case Cons(_, xs) => filter(xs)(f)
  }

  def flatMap[A, B](as: SimpleList[A])(f: A => SimpleList[B]): SimpleList[B] = foldRight(as, Nil: SimpleList[B])((h, t) => append(f(h), t))

  def filter2[A](as: SimpleList[A])(f: A => Boolean): SimpleList[A] = flatMap(as)(x => if (f(x)) SimpleList(x) else Nil)

  def addPairwise(a: SimpleList[Int], b: SimpleList[Int]): SimpleList[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(ah, at), Cons(bh, bt)) => Cons(ah + bh, addPairwise(at, bt))
  }
}