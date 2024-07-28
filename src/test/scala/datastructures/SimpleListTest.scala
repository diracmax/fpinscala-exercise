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

  test("Exercise 3.3: List.setHead should return the list with the new head") {
    assert(SimpleList.setHead(10, SimpleList(1, 2, 3, 4, 5)) == SimpleList(10, 2, 3, 4, 5))
  }

  test("Exercise 3.4: List.drop should return the list without the first n elements") {
    assert(SimpleList.drop(SimpleList(1, 2, 3, 4, 5), 2) == SimpleList(3, 4, 5))
  }
}
