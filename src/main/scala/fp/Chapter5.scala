package fp

object Chapter5 {
  sealed trait MyStream[+A] {
    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    def take(n: Int): MyStream[A] = this match {
      case Empty => MyStream.empty[A]
      case Cons(h, t) =>
        if (n > 0)
          Cons(h, () => t().take(n - 1))
        else
          MyStream.empty[A]
    }

    def drop(n: Int): MyStream[A] = this match {
      case Empty => MyStream.empty[A]
      case stream@Cons(h, t) =>
        if (n > 0)
          t().drop(n - 1)
        else
          stream
    }

    def takeWhile(p: A => Boolean): MyStream[A] = this match {
      case Empty => MyStream.empty[A]
      case stream@Cons(h, t) =>
        val a = h()
        if (p(a)) Cons(h, () => t().takeWhile(p)) else MyStream.empty
    }

    def forAll(p: A => Boolean): Boolean = this match {
      case Empty => true
      case Cons(h, t) => p(h()) && t().forAll(p)
    }
  }

  case object Empty extends MyStream[Nothing]
  case class Cons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

  object MyStream {
    def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A]: MyStream[A] = Empty
    def apply[A](as: A*): MyStream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
}
