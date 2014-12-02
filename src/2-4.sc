def uncurry[A,B,C](f: A => B => C): (A, B) => C = (x, y) => f(x)(y)
