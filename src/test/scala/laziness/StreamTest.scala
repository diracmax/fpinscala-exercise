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

  test("Exercise 5.7 map should return the stream with the function applied to the values") {
    assert(Stream(1, 2, 3).map(_ + 1).toList == List(2, 3, 4))
    assert(Stream[Int]().map(_ + 1).toList == Nil)
  }

  test("Exercise 5.7 filter should return the stream with the values that satisfy the condition") {
    assert(Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList == List(2, 4))
    assert(Stream(1, 2, 3, 4, 5).filter(_ % 2 == 1).toList == List(1, 3, 5))
    assert(Stream[Int]().filter(_ % 2 == 0).toList == Nil)
  }

  test("Exercise 5.7 append should return the stream with the appended stream") {
    assert(Stream(1, 2, 3).append(Stream(4, 5)).toList == List(1, 2, 3, 4, 5))
    assert(Stream(1, 2, 3).append(Stream[Int]()).toList == List(1, 2, 3))
    assert(Stream[Int]().append(Stream(4, 5)).toList == List(4, 5))
  }

  test("Exercise 5.7 flatMap should return the stream with the function applied to the values") {
    assert(Stream(1, 2, 3).flatMap(a => Stream(a, a)).toList == List(1, 1, 2, 2, 3, 3))
    assert(Stream[Int]().flatMap(a => Stream(a, a)).toList == Nil)
  }

  test("Exercise 5.8 constant should return the stream with the constant value") {
    assert(Stream.constant(1).take(3).toList == List(1, 1, 1))
    assert(Stream.constant(1).take(0).toList == Nil)
  }
}
