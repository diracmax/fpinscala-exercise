package laziness

import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.{Stream => _}

class StreamTest extends AnyFunSuite {
  test("Exercise 5.1 toList should return the list of values") {
    assert(Stream(1, 2, 3).toList == List(1, 2, 3))
    assert(Stream().toList == Nil)
  }
}
