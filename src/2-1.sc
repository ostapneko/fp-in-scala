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

fib(1)
fib(2)
fib(3)
fib(4)
fib(5)
fib(6)
