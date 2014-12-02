def setHead[A](xs: List[A], h: A): List[A] = xs match {
  case Nil => throw new RuntimeException("Replacing head of an empty list")
  case y :: ys => h :: ys
}
