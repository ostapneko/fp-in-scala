def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  as.length < 2 || ordered(as(0), as(1)) && isSorted(as.tail, ordered)
}

def ord(n1: Int, n2: Int): Boolean = n1 <= n2

assert(isSorted[Int](Array(1,2,3,3), ord))
assert(!isSorted[Int](Array(1,4,3,3), ord))
assert(isSorted[Int](Array(), ord))
assert(isSorted[Int](Array(1), ord))
