def init[A](l: List[A]): List[A] = l match {
  case Nil => throw new RuntimeException("init of empty List")
  case List(x) => Nil
  case x :: xs => x :: init(xs)
}

assert(init(List(1,2,3,4)) == List(1,2,3))
