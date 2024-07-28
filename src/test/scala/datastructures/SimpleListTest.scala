package datastructures

import org.scalatest.funsuite.AnyFunSuite

class SimpleListTest extends AnyFunSuite{
  test("List.sum should return sum of the list") {
    assert(SimpleList.sum(SimpleList(1, 2, 3, 4, 5)) == 15)
  }

  test("List.product should return product of the list") {
    assert(SimpleList.product(SimpleList(1, 2, 3, 4, 5)) == 120)
  }

  test("Exercise 3.1: List should return the correct value") {
    assert(SimpleList.x == 3)
  }

  test("Exercise 3.2: List.tail should return the tail of the list") {
    assert(SimpleList.tail(SimpleList(1, 2, 3, 4, 5)) == SimpleList(2, 3, 4, 5))
  }
}
