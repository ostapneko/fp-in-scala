package fp

object Chapter5 {
  sealed trait StreamCustom[+A] {
    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    def take(n: Int): StreamCustom[A] = this match {
      case Empty => StreamCustom.empty[A]
      case Cons(h, t) =>
        if (n > 0)
          Cons(h, () => t().take(n - 1))
        else
          StreamCustom.empty[A]
    }

    def drop(n: Int): StreamCustom[A] = this match {
      case Empty => StreamCustom.empty[A]
      case stream@Cons(h, t) =>
        if (n > 0)
          t().drop(n - 1)
        else
          stream
    }
  }

  case object Empty extends StreamCustom[Nothing]
  case class Cons[+A](h: () => A, t: () => StreamCustom[A]) extends StreamCustom[A]

  object StreamCustom {
    def cons[A](hd: => A, tl: => StreamCustom[A]): StreamCustom[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A]: StreamCustom[A] = Empty
    def apply[A](as: A*): StreamCustom[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
}
