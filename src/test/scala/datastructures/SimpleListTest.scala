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

  test("Exercise 3.11 sum should return the sum of the list") {
    assert(SimpleList.sum2(SimpleList(1, 2, 3, 4, 5)) == 15)
  }

  test("Exercise 3.11 product should return the product of the list") {
    assert(SimpleList.product2(SimpleList(1, 2, 3, 4, 5)) == 120)
  }

  test("Exercise 3.11 length should return the length of the list") {
    assert(SimpleList.length2(SimpleList(1, 2, 3, 4, 5)) == 5)
  }

  test("Exercise 3.12 reverse should return the reversed list") {
    assert(SimpleList.reverse(SimpleList(1, 2, 3, 4, 5)) == SimpleList(5, 4, 3, 2, 1))
  }

  test("Exercise 3.14 append should return the appended list") {
    assert(SimpleList.append(SimpleList(1, 2, 3), SimpleList(4, 5)) == SimpleList(1, 2, 3, 4, 5))
  }

  test("Exercise 3.15 concat should return the concatenated list") {
    assert(SimpleList.concat(SimpleList(SimpleList(1, 2, 3), SimpleList(4, 5))) == SimpleList(1, 2, 3, 4, 5))
  }

  test("Exercise 3.16 plus1 should return the list with 1 plus each element") {
    assert(SimpleList.plus1(SimpleList(1, 2, 3, 4, 5)) == SimpleList(2, 3, 4, 5, 6))
  }

  test("Exercise 3.17 doubleToString should return the list with each element converted to string") {
    assert(SimpleList.doubleToString(SimpleList(1.0, 2.0, 3.0, 4.0, 5.0)) == SimpleList("1.0", "2.0", "3.0", "4.0", "5.0"))
  }

  test("Exercise 3.18 map should return the list with each element transformed by the function") {
    assert(SimpleList.map(SimpleList(1, 2, 3, 4, 5))(_ + 1) == SimpleList(2, 3, 4, 5, 6))
  }
}
