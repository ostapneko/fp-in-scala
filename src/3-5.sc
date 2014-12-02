def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
  case Nil => Nil
  case x :: xs => if (f(x)) dropWhile(xs, f) else l
}

assert(dropWhile[Int](List(1,2,3,4), n => n < 2) == List(2,3,4))
