package preparation

import org.scalatest.funsuite.AnyFunSuite

class CurryTest extends AnyFunSuite{
  test("Curry.curry should return curried function") {
    val f = (a: Int, b: Int) => a + b
    val curried = Curry.curry(f)
    assert(curried(1)(2) == 3)
  }

  test("Curry.uncurry should return uncurried function") {
    val f = (a: Int) => (b: Int) => a + b
    val uncurried = Curry.uncurry(f)
    assert(uncurried(1, 2) == 3)
  }
}
