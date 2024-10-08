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

  test("Exercise 3.27: depth should return the maximum path length from the root of the tree to any leaf") {
    assert(Tree.depth(Branch(Leaf(1), Leaf(2))) == 1)
    assert(Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) == 2)
    assert(Tree.depth(Branch(Branch(Leaf(1), Branch(Leaf(1), Leaf(2))), Leaf(3))) == 3)
  }

  test("Exercise 3.28: map should return a new tree with the function applied to each element") {
    assert(Tree.map(Branch(Leaf(1), Leaf(2)))(_ + 1) == Branch(Leaf(2), Leaf(3)))
    assert(Tree.map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(_ + 1) == Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
  }

  test("Exercise 3.29: fold should return the correct value") {
    assert(Tree.size2(Branch(Leaf(1), Leaf(2))) == 3)
    assert(Tree.maximum2(Branch(Leaf(1), Leaf(2))) == 2)
    assert(Tree.depth2(Branch(Leaf(1), Leaf(2))) == 1)
    assert(Tree.map2(Branch(Leaf(1), Leaf(2)))(_ + 1) == Branch(Leaf(2), Leaf(3)))
  }
}
