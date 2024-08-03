package datastructures

import org.scalatest.funsuite.AnyFunSuite

class TreeTest extends AnyFunSuite {
  test("Exercise 3.25: size should return the number of nodes in the tree") {
    assert(Tree.size(Branch(Leaf(1), Leaf(2))) == 3)
    assert(Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) == 5)
    assert(Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(2)))) == 7)
  }
}
