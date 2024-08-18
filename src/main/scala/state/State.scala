package state

import state.RNG.Rand

import scala.math.abs

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, newRNG) = rng.nextInt
    (abs(n).toDouble / Int.MaxValue, newRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count > 0) {
      val (n, newRNG) = rng.nextInt
      val (l, r) = ints(count - 1)(newRNG)
      (n :: l, r)
    } else {
      (Nil, rng)
    }
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  val double: Rand[Double] = rng => {
    val (n, newRNG) = rng.nextInt
    (abs(n).toDouble / Int.MaxValue, newRNG)
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (newInt, newRNG) = rng.nextInt
    if (newInt < 0) {
      (-(newInt+1), newRNG)
    } else {
      (newInt, newRNG)
    }
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def doubleViaMap: Rand[Double] = map(int)(x => abs(x).toDouble / Int.MaxValue)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => fs match {
      case h :: t =>
        val (e, r) = h(rng)
        val (e1, r1) = sequence(t)(r)
        (e :: e1, r1)
      case Nil => (Nil, rng)
    }

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (e, r) = f(rng)
      g(e)(r)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt){ i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }
}