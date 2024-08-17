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
}
