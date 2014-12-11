def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
  as match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }

def append[A](left: List[A], right: List[A]): List[A] =
  foldRight(left, right)(_ :: _)

def concat[A](ls: List[List[A]]): List[A] =
  foldRight(ls, List.empty[A])(append)

assert(concat(List(List(1,2), List(3,4))) == List(1,2,3,4))
