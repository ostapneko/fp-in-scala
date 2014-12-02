def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
  as match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }

def length[A](as: List[A]): Int = foldRight(as, 0)( (_, n) => n + 1)

//stack overflow
//length((1 to 1000000).toList)


def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
  as match {
    case Nil => z
    case x :: xs => foldLeft(xs, f(z, x))(f)
  }
}


def length2[A](as: List[A]): Int = foldLeft(as, 0)((n, _) => n + 1)
length2((1 to 1000000).toList)
