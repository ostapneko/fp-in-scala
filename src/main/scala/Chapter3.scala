import scala.annotation.tailrec

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

  def foldRight1[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case x :: xs => f(x, foldRight1(xs, z)(f))
    }

  def foldLeft1[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case x :: xs => foldLeft1(xs, f(z, x))(f)
    }
  }

  def sum1(ns: List[Int]) = foldLeft1(ns, 0)(_ + _)

  def product1(ns: List[Int]) = foldLeft1(ns, 1)(_ * _)

  def length1[A](as: List[A]): Int = foldLeft1(as, 0)((n, _) => n + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft1(as, List[A]())((acc, x) => x :: acc)

  def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight1(as, z)((y, x) => f(x, y))
  }

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft1(as, z)((y, x) => f(x, y))
  }

  def sum2(ns: List[Int]) = foldLeft2(ns, 0)(_ + _)
  def product2(ns: List[Int]) = foldLeft2(ns, 1)(_ * _)
  def length2[A](as: List[A]): Int = foldLeft2(as, 0)((n, _) => n + 1)

  def append[A](left: List[A], right: List[A]): List[A] =
    foldRight1(left, right)(_ :: _)

  def concat[A](ls: List[List[A]]): List[A] =
    foldRight1(ls, List.empty[A])(append)

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
