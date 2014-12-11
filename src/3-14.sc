def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
  as match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }

def append[A](left: List[A], right: List[A]): List[A] =
  foldRight(left, right)(_ :: _)

assert(append(List(1,2,3), List(4,5)) == List(1,2,3,4,5))
