package laziness

import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.{Stream => _}

class StreamTest extends AnyFunSuite {
  test("Exercise 5.1 toList should return the list of values") {
    assert(Stream(1, 2, 3).toList == List(1, 2, 3))
    assert(Stream().toList == Nil)
  }

  test("Exercise 5.2 take should return the first n values") {
    assert(Stream(1, 2, 3).take(2).toList == List(1, 2))
    assert(Stream(1, 2, 3).take(4).toList == List(1, 2, 3))
    assert(Stream(1, 2, 3).take(0).toList == Nil)
  }

  test("Exercise 5.2 drop should return the stream without the first n values") {
    assert(Stream(1, 2, 3).drop(2).toList == List(3))
    assert(Stream(1, 2, 3).drop(4).toList == Nil)
    assert(Stream(1, 2, 3).drop(0).toList == List(1, 2, 3))
  }
}
