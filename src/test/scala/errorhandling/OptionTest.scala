package errorhandling

import scala.{Option => _, Some => _, None => _}
import org.scalatest.funsuite.AnyFunSuite

class OptionTest extends AnyFunSuite {
  test("map should return Some with the function applied to the value") {
    assert(Some(1).map(_ + 1) == Some(2))
    assert(None.map((a: Int) => a + 1) == None)
  }

  test("flatMap should return Some with the function applied to the value") {
    assert(Some(1).flatMap(a => Some(a + 1)) == Some(2))
    assert(None.flatMap((a: Int) => Some(a + 1)) == None)
  }

  test("getOrElse should return the value if it exists or the default value") {
    assert(Some(1).getOrElse(0) == 1)
    assert(None.getOrElse(0) == 0)
  }

  test("orElse should return the value if it exists or the default Option") {
    assert(Some(1).orElse(Some(0)) == Some(1))
    assert(None.orElse(Some(0)) == Some(0))
  }

  test("filter should return the value if it exists and satisfies the condition") {
    assert(Some(1).filter(_ == 1) == Some(1))
    assert(Some(1).filter(_ == 2) == None)
    assert(None.filter((a: Int) => a == 1) == None)
  }

  test("Exercise 4.2: variance should return the variance of the sequence") {
    assert(Option.variance(Seq(1, 2, 3, 4, 5)) == Some(2.0))
    assert(Option.variance(Seq()) == None)
  }

  test("map2 should return Some with the function applied to the values") {
    assert(Option.map2(Some(1), Some(2))(_ + _) == Some(3))
    assert(Option.map2(Some(1), None)(_ + _) == None)
    assert(Option.map2(None, Some(2))((a: Int, b: Int) => a + b) == None)
  }

  test("sequence should return Some with the list of values if all values are Some") {
    assert(Option.sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
    assert(Option.sequence(List(Some(1), None, Some(3))) == None)
    assert(Option.sequence(Nil) == Some(Nil))
  }

  test("Exercise 4.5 traverse should return Some with the list of values if all values are Some") {
    assert(Option.traverse(List(1, 2, 3))(a => Some(a + 1)) == Some(List(2, 3, 4)))
    assert(Option.traverse(List(1, 2, 3))(a => if (a % 2 == 0) Some(a) else None) == None)
    assert(Option.traverse(Nil)(Some(_)) == Some(Nil))
  }
}
