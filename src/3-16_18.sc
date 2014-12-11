import scala.annotation.tailrec

def plusOne(ls: List[Int]): List[Int] = ls match {
  case Nil => Nil
  case x :: xs => x + 1 :: plusOne(xs)
}


assert(plusOne(List(1,2)) == List(2,3))

def doubleToString(ls: List[Double]): List[String] = ls match {
  case Nil => Nil
  case x :: xs => x.toString :: doubleToString(xs)
}

assert(doubleToString(List(1.0,2.0)) == List("1.0","2.0"))

def map[A,B](as: List[A])(f: A => B): List[B] = {
  @tailrec
  def go(acc: List[B], rest: List[A]): List[B] = rest match {
    case Nil => acc
    case x :: xs => go(f(x) :: acc, xs)
  }

  go(List.empty[B], as).reverse
}

assert(map(List(1,2))(_ + 1) == List(2,3))
