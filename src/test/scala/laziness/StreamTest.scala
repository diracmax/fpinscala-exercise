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

  test("Exercise 5.3 takeWhile should return the stream while the condition is satisfied") {
    assert(Stream(1, 2, 3, 4, 5).takeWhile(_ < 4).toList == List(1, 2, 3))
    assert(Stream(1, 2, 3, 4, 5).takeWhile(_ < 6).toList == List(1, 2, 3, 4, 5))
    assert(Stream(1, 2, 3, 4, 5).takeWhile(_ < 1).toList == Nil)
    assert(Stream[Int]().takeWhile(_ < 0).toList == Nil)
  }

  test("Exercise 5.4 forAll should return true if all elements satisfy the condition") {
    assert(Stream(1, 2, 3, 4, 5).forAll(_ < 6))
    assert(!Stream(1, 2, 3, 4, 5).forAll(_ < 5))
    assert(!Stream(1, 2, 3, 4, 5).forAll(_ < 4))
    assert(!Stream(1, 2, 3, 4, 5).forAll(_ < 3))
    assert(!Stream(1, 2, 3, 4, 5).forAll(_ < 2))
    assert(!Stream(1, 2, 3, 4, 5).forAll(_ < 1))
    assert(Stream[Int]().forAll(_ < 0))
  }

  test("Exercise 5.5 takeWhile2 should return the stream while the condition is satisfied") {
    assert(Stream(1, 2, 3, 4, 5).takeWhile2(_ < 4).toList == List(1, 2, 3))
    assert(Stream(1, 2, 3, 4, 5).takeWhile2(_ < 6).toList == List(1, 2, 3, 4, 5))
    assert(Stream(1, 2, 3, 4, 5).takeWhile2(_ < 1).toList == Nil)
    assert(Stream[Int]().takeWhile2(_ < 0).toList == Nil)
  }
}
