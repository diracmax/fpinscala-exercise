package state

import org.scalatest.funsuite.AnyFunSuite

class StateTest extends AnyFunSuite {
  test("Exercise 6.1 nonNegativeInt should return a non-negative integer") {
    val rng = SimpleRNG(42)
    val (n1, rng2) = RNG.nonNegativeInt(rng)
    assert(n1 >= 0)
    val (n2, _) = RNG.nonNegativeInt(rng2)
    assert(n2 >= 0)
  }

  test("Exercise 6.2 double should return a double between 0 and 1") {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.double(rng)
    assert(n1 >= 0.0 && n1 < 1.0)
    val (n2, _) = rng.double(rng2)
    assert(n2 >= 0.0 && n2 < 1.0)
  }

  test("Exercise 6.3 intDouble should return a pair of integer and double") {
    val rng = SimpleRNG(42)
    val ((i1, d1), rng2) = rng.intDouble(rng)
    assert(i1 >= Int.MinValue && i1 <= Int.MaxValue)
    assert(d1 >= 0.0 && d1 < 1.0)
    val ((i2, d2), _) = rng.intDouble(rng2)
    assert(i2 >= Int.MinValue && i2 <= Int.MaxValue)
    assert(d2 >= 0.0 && d2 < 1.0)
  }

  test("Exercise 6.3 doubleInt should return a pair of double and integer") {
    val rng = SimpleRNG(42)
    val ((d1, i1), rng2) = rng.doubleInt(rng)
    assert(i1 >= Int.MinValue && i1 <= Int.MaxValue)
    assert(d1 >= 0.0 && d1 < 1.0)
    val ((d2, i2), _) = rng.doubleInt(rng2)
    assert(i2 >= Int.MinValue && i2 <= Int.MaxValue)
    assert(d2 >= 0.0 && d2 < 1.0)
  }

  test("Exercise 6.3 double3 should return a triple of doubles") {
    val rng = SimpleRNG(42)
    val ((d1, d2, d3), rng2) = rng.double3(rng)
    assert(d1 >= 0.0 && d1 < 1.0)
    assert(d2 >= 0.0 && d2 < 1.0)
    assert(d3 >= 0.0 && d3 < 1.0)
    val ((d4, d5, d6), _) = rng.double3(rng2)
    assert(d4 >= 0.0 && d4 < 1.0)
    assert(d5 >= 0.0 && d5 < 1.0)
    assert(d6 >= 0.0 && d6 < 1.0)
  }

  test("Exercise 6.4 ints should return a list of random integers") {
    val rng = SimpleRNG(42)
    val (l1, rng2) = rng.ints(5)(rng)
    assert(l1 == List(16159453, -1281479697, -340305902, -2015756020, 1770001318))
    println(l1)
    val (l2, _) = rng.ints(5)(rng2)
    assert(l2.length == 5)
    assert(l2 == List(-1934589059, 1015914512, -1163632441, -94901159, 1837487774))
  }
}
