package datastructures

import org.scalatest.funsuite.AnyFunSuite

class SimpleListTest extends AnyFunSuite{
  test("sum should return sum of the list") {
    assert(SimpleList.sum(SimpleList(1, 2, 3, 4, 5)) == 15)
  }

  test("product should return product of the list") {
    assert(SimpleList.product(SimpleList(1, 2, 3, 4, 5)) == 120)
  }

  test("Exercise 3.1") {
    assert(SimpleList.x == 3)
  }

  test("Exercise 3.2: tail should return the tail of the list") {
    assert(SimpleList.tail(SimpleList(1, 2, 3, 4, 5)) == SimpleList(2, 3, 4, 5))
  }

  test("Exercise 3.3: setHead should return the list with the new head") {
    assert(SimpleList.setHead(10, SimpleList(1, 2, 3, 4, 5)) == SimpleList(10, 2, 3, 4, 5))
  }

  test("Exercise 3.4: drop should return the list without the first n elements") {
    assert(SimpleList.drop(SimpleList(1, 2, 3, 4, 5), 2) == SimpleList(3, 4, 5))
  }

  test("Exercise 3.5: dropWhile should return the list without the elements that satisfy the condition") {
    assert(SimpleList.dropWhile(SimpleList(1, 2, 3, 4, 5), (a: Int) => a < 3) == SimpleList(3, 4, 5))
  }

  test("Exercise 3.6: init should return the list without the last element") {
    assert(SimpleList.init(SimpleList(1, 2, 3, 4, 5)) == SimpleList(1, 2, 3, 4) )
  }

  test("foldRight should return the correct value") {
    assert(SimpleList.foldRight(SimpleList(1, 2, 3, 4, 5), 0)(_ + _) == 15)
  }

  test("Exercise 3.8") {
    assert(SimpleList.y == SimpleList(1, 2, 3))
  }

  test("Exercise 3.9 length should return the length of the SimpleList") {
    assert(SimpleList.length(SimpleList(1, 2, 3, 4, 5)) == 5)
  }

  test("Exercise 3.10 foldLeft should return the correct value") {
    assert(SimpleList.foldLeft(SimpleList(1, 2, 3, 4, 5), 0)(_ + _) == 15)
  }
}
