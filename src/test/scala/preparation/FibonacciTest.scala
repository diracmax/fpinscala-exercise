package preparation

import org.scalatest.funsuite.AnyFunSuite

class FibonacciTest extends AnyFunSuite {
  test("Fibonacci.fib should return fibonacci number") {
    assert(Fibonacci.fib(0) == 0)
    assert(Fibonacci.fib(1) == 1)
    assert(Fibonacci.fib(2) == 1)
    assert(Fibonacci.fib(3) == 2)
    assert(Fibonacci.fib(4) == 3)
    assert(Fibonacci.fib(5) == 5)
    assert(Fibonacci.fib(6) == 8)
    assert(Fibonacci.fib(7) == 13)
  }
}
