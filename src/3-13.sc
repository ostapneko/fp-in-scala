def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
  as match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }

def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
  as match {
    case Nil => z
    case x :: xs => foldLeft(xs, f(z, x))(f)
  }
}

def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
  foldRight(as, z)((y, x) => f(x, y))
}

def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
  foldLeft(as, z)((y, x) => f(x, y))
}

def sum(ns: List[Int]) = foldLeft2(ns, 0)(_ + _)
def product(ns: List[Int]) = foldLeft2(ns, 1)(_ * _)
def length[A](as: List[A]): Int = foldLeft2(as, 0)((n, _) => n + 1)

sum(List(1,2,3))
