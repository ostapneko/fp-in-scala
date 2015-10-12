object Chapter3 {
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => throw new RuntimeException("Tail of empty list")
    case y :: ys => ys
  }

  def setHead[A](xs: List[A], h: A): List[A] = xs match {
    case Nil => throw new RuntimeException("Replacing head of an empty list")
    case y :: ys => h :: ys
  }

  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (_, 0) => l
    case (Nil, _) => Nil
    case (x :: xs, _) => drop(xs, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case x :: xs => if (f(x)) dropWhile(xs, f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new RuntimeException("init of empty List")
    case List(x) => Nil
    case x :: xs => x :: init(xs)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case x :: xs => f(x, foldRight(xs, z)(f))
    }

  def length[A](as: List[A]): Int = foldRight(as, 0)( (_, n) => n + 1)

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case x :: xs => foldLeft(xs, f(z, x))(f)
    }
  }

  def length2[A](as: List[A]): Int = foldLeft(as, 0)((n, _) => n + 1)
  length2((1 to 1000000).toList)
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case x :: xs => foldLeft(xs, f(z, x))(f)
    }
  }

  def sum(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def product(ns: List[Int]) = foldLeft(ns, 1)(_ * _)

  def length[A](as: List[A]): Int = foldLeft(as, 0)((n, _) => n + 1)

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case x :: xs => foldLeft(xs, f(z, x))(f)
    }
  }

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((acc, x) => x :: acc)

  def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, z)((y, x) => f(x, y))
  }

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as, z)((y, x) => f(x, y))
  }

  def sum(ns: List[Int]) = foldLeft2(ns, 0)(_ + _)
  def product(ns: List[Int]) = foldLeft2(ns, 1)(_ * _)
  def length[A](as: List[A]): Int = foldLeft2(as, 0)((n, _) => n + 1)

  def append[A](left: List[A], right: List[A]): List[A] =
    foldRight(left, right)(_ :: _)

  def concat[A](ls: List[List[A]]): List[A] =
    foldRight(ls, List.empty[A])(append)

  def plusOne(ls: List[Int]): List[Int] = ls match {
    case Nil => Nil
    case x :: xs => x + 1 :: plusOne(xs)
  }

  def doubleToString(ls: List[Double]): List[String] = ls match {
    case Nil => Nil
    case x :: xs => x.toString :: doubleToString(xs)
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    @tailrec
    def go(acc: List[B], rest: List[A]): List[B] = rest match {
      case Nil => acc
      case x :: xs => go(f(x) :: acc, xs)
    }

    go(List.empty[B], as).reverse
  }
}
