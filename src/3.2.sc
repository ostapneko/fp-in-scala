def tail[A](xs: List[A]): List[A] = xs match {
  case Nil => throw new RuntimeException("Tail of empty list")
  case y :: ys => ys
}
