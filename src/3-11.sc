def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
  as match {
    case Nil => z
    case x :: xs => foldLeft(xs, f(z, x))(f)
  }
}

def sum(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
def product(ns: List[Int]) = foldLeft(ns, 1)(_ * _)
def length[A](as: List[A]): Int = foldLeft(as, 0)((n, _) => n + 1)
