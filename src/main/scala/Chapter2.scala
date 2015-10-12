object Chapter2 {
  def fib(n: Int): Int = {
    def fib2(n1: Int, n2: Int, p: Int): Int = {
      if (p >= n)
        n2
      else
        fib2(n2, n1 + n2, p + 1)
    }
    if (n == 1)
      0
    else
      fib2(0, 1, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    as.length < 2 || ordered(as(0), as(1)) && isSorted(as.tail, ordered)
  }

  def ord(n1: Int, n2: Int): Boolean = n1 <= n2

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = x => {
    y => f(x, y)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (x, y) => f(x)(y)

  def compose[A,B,C](f: B => C, g: A => B): A => C = x => f(g(x))
}
