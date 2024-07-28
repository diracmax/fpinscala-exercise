package preparation

object Sort {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    for (i <- 0 until as.length - 1) {
      if (!ordered(as(i), as(i+1))) {
        return false
      }
    }
    true
  }
}
