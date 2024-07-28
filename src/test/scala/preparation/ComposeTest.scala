package preparation

import org.scalatest.funsuite.AnyFunSuite

class ComposeTest extends AnyFunSuite{
  test("Compose.compose should return composed function") {
    val f = (a: Int) => a + 1
    val g = (a: Int) => a * 2
    val composed = Compose.compose(f, g)
    assert(composed(1) == 3)
  }
}
