import org.scalatest.funsuite.AnyFunSuite

class SetSuite extends AnyFunSuite {

  test("An empty Set should have size 0") {
    assert(Set.empty.isEmpty)
  }

  test("Invoking head on an empty Set should produce NoSuchElementException") {
    assertThrows[NoSuchElementException] {
      Set.empty.head
    }
  }
}