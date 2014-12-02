def compose[A,B,C](f: B => C, g: A => B): A => C = x => f(g(x))
