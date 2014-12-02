def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
  case (_, 0) => l
  case (Nil, _) => Nil
  case (x :: xs, _) => drop(xs, n - 1)
}

assert(drop(List(1,2,3), 2) == List(3))
