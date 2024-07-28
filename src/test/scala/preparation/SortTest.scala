package preparation

import org.scalatest.funsuite.AnyFunSuite

class SortTest extends AnyFunSuite{
  test("Sort.isSorted should return true if the array is sorted") {
    assert(Sort.isSorted(Array(1, 2, 3, 4, 5), (a: Int, b: Int) => a < b))
    assert(Sort.isSorted(Array(1, 1, 1, 1, 1), (a: Int, b: Int) => a <= b))
    assert(Sort.isSorted(Array(5, 4, 3, 2, 1), (a: Int, b: Int) => a > b))
  }

  test("Sort.isSorted should return false if the array is not sorted") {
    assert(!Sort.isSorted(Array(1, 2, 3, 4, 5), (a: Int, b: Int) => a > b))
    assert(!Sort.isSorted(Array(1, 1, 1, 1, 1), (a: Int, b: Int) => a < b))
    assert(!Sort.isSorted(Array(5, 4, 3, 2, 1), (a: Int, b: Int) => a < b))
  }
}
