package datastructures

import org.scalatest.funsuite.AnyFunSuite

class TreeTest extends AnyFunSuite {
  test("Exercise 3.25: size should return the number of nodes in the tree") {
    assert(Tree.size(Branch(Leaf(1), Leaf(2))) == 3)
    assert(Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) == 5)
    assert(Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(2)))) == 7)
  }

  test("Exercise 3.26: maximum should return the maximum value in the tree") {
    assert(Tree.maximum(Branch(Leaf(1), Leaf(2))) == 2)
    assert(Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) == 3)
    assert(Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) == 4)
  }
}
