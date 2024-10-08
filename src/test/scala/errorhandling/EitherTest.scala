package errorhandling

import org.scalatest.funsuite.AnyFunSuite

class EitherTest extends AnyFunSuite {
  test("Exercise 4.6 map should return Right with the function applied to the value") {
    assert(Right(1).map(_ + 1) == Right(2))
    assert((Left("error"): Either[String, Int]).map((a: Int) => a + 1) == Left("error"))
  }

  test("Exercise 4.6 flatMap should return Right with the function applied to the value") {
    assert(Right(1).flatMap(a => Right(a + 1)) == Right(2))
    assert((Left("error"): Either[String, Int]).flatMap((a: Int) => Right(a + 1)) == Left("error"))
  }

  test("Exercise 4.6 orElse should return the value if it exists or the default Either") {
    assert(Right(1).orElse(Right(0)) == Right(1))
    assert((Left("error"): Either[String, Int]).orElse(Right(0)) == Right(0))
  }

  test("Exercise 4.6 map2 should return Right with the function applied to the values") {
    assert(Right(1).map2(Right(2))(_ + _) == Right(3))
    assert(Right(1).map2(Left("error"): Either[String, Int])((a: Int, b: Int) => a + b) == Left("error"))
    assert((Left("error"): Either[String, Int]).map2(Right(2))((a: Int, b: Int) => a + b) == Left("error"))
    assert((Left("error1"): Either[String, Int]).map2(Left("error2"): Either[String, Int])((a: Int, b: Int) => a + b) == Left("error1"))
  }

  test("Exercise 4.7 sequence should return Right with the list of values if all values are Right") {
    assert(Either.sequence(List(Right(1), Right(2), Right(3))) == Right(List(1, 2, 3)))
    assert(Either.sequence(List(Right(1), Left("error"), Right(3))) == Left("error"))
    assert(Either.sequence(List(Right(1), Left("error1"), Left("error2"))) == Left("error1"))
    assert(Either.sequence(Nil) == Right(Nil))
  }

  test("Exercise 4.7 traverse should return Right with the list of values if all values are Right") {
    assert(Either.traverse(List(1, 2, 3))(a => Right(a + 1)) == Right(List(2, 3, 4)))
    assert(Either.traverse(List(1, 2, 3))(a => if (a % 2 == 0) Right(a) else Left("error")) == Left("error"))
    assert(Either.traverse(Nil)(Right(_)) == Right(Nil))
  }
}
