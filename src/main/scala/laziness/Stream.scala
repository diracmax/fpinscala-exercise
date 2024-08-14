package laziness

import scala.collection.immutable.{Stream => _}

sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if (p(h()))  Cons(h, () => t().takeWhile(p)) else t().takeWhile(p)
    case _ => Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((x, y) => p(x) && y)

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((x, y) => if (p(x)) Cons(() => x, () => y) else Empty)

  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((x, y) => Cons(() => f(x), () => y))

  def filter(f: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((x, y) => if (f(x)) Cons(() => x, () => y) else y)

  def append[A2>:A](as: => Stream[A2]): Stream[A2] = foldRight(as)((a, b) => Cons(() => a, () => b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((a, b) => f(a).append(b))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    lazy val is: Stream[A] = cons(a, is)
    is
  }
}