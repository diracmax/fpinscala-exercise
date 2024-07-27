package preparation

object Fibonacci {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def fibTailCall(a0: Int, a1: Int, n: Int): Int = n match {
      case 0 => a0
      case 1 => a1
      case _ => fibTailCall(a1, a0 + a1, n - 1)
    }
    fibTailCall(0, 1, n)
  }
}