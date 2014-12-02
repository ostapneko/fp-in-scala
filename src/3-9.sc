def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
  as match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }

def length[A](as: List[A]): Int = foldRight(as, 0)( (_, n) => n + 1)

assert(length(List(1,2,3)) == 3)
