package state

import org.scalatest.funsuite.AnyFunSuite

class StateTest extends AnyFunSuite {
  test("Exercise 6.1 nonNegativeInt should return a non-negative integer") {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.nonNegativeInt(rng)
    assert(n1 >= 0)
    val (n2, _) = rng.nonNegativeInt(rng2)
    assert(n2 >= 0)
  }

  test("Exercise 6.2 double should return a double between 0 and 1") {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.double(rng)
    assert(n1 >= 0.0 && n1 < 1.0)
    val (n2, _) = rng.double(rng2)
    assert(n2 >= 0.0 && n2 < 1.0)
  }
}
