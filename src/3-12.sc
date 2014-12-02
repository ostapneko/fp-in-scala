def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
  as match {
    case Nil => z
    case x :: xs => foldLeft(xs, f(z, x))(f)
  }
}

def reverse[A](as: List[A]): List[A] =
  foldLeft(as, List[A]())((acc, x) => x :: acc)

assert(reverse(List(1,2,3))== List(3,2,1))
