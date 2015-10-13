package fp

object Chapter4 {
  trait Option[+A] {
    def map[B](f: A => B): Option[B]
    def flatMap[B](f: A => Option[B]): Option[B]
    def getOrElse[B >: A](default: => B): B
    def orElse[B >: A](ob: => Option[B]): Option[B]
    def filter(f: A => Boolean): Option[A]
  }

  case class Some[+A](get: A) extends Option[A] {
    override def map[B](f: (A) => B): Option[B] = Some(f(get))
    override def flatMap[B](f: (A) => Option[B]): Option[B] = f(get)
    override def getOrElse[B >: A](default: => B): B = get
    override def orElse[B >: A](ob: => Option[B]): Option[B] = this
    override def filter(f: (A) => Boolean): Option[A] = if (f(get)) this else None
  }

  case object None extends Option[Nothing] {
    override def map[B](f: (Nothing) => B): Option[B] = None
    override def flatMap[B](f: (Nothing) => Option[B]): Option[B] = None
    override def getOrElse[B >: Nothing](default: => B): B = default
    override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
    override def filter(f: (Nothing) => Boolean): Option[Nothing] = None
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    val mean = if (xs.isEmpty)
      None
    else
      Some(xs.sum / xs.size)

    mean.map { m =>
      val sum = xs.map { x =>
        math.pow(x - m, 2)
      }.sum
      sum / xs.length
    }
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      x1 <- a
      x2 <- b
    } yield f(x1, x2)
  }

  def sequence[A](as: List[Option[A]]): Option[List[A]] = as.foldRight[Option[List[A]]](Some(Nil)) { (a, acc) =>
    map2(a, acc) { _ :: _ }
  }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as.foldLeft[Option[List[B]]](Some(Nil)) { (accOpt, a) =>
    val reversed = for {
      acc <- accOpt
      b <- f(a)
    } yield b :: acc

    reversed.map(_.reverse)
  }

  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(x) => Right(f(x))
      case Left(e) => Left(e)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(x) => f(x)
      case Left(e) => Left(e)
    }

    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(x) => Right(x)
      case _ => b
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        x1 <- this
        x2 <- b
      } yield f(x1, x2)
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  def sequenceEither[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es.foldRight[Either[E, List[A]]](Right(Nil)) { (a, acc) =>
    a.map2(acc) { _ :: _ }
  }


  def traverseEither[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as.foldLeft[Either[E, List[B]]](Right(Nil)) { (accOpt, a) =>
    val reversed = for {
      acc <- accOpt
      b <- f(a)
    } yield b :: acc

    reversed.map(_.reverse)
  }

  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  trait Validation[+E, +A] {
    def map[B](f: A => B): Validation[E, B] = this match {
      case VSuccess(x) => VSuccess(f(x))
      case VError(es) => VError(es)
    }

    def flatMap[EE >: E, B](f: A => Validation[EE, B]): Validation[EE, B] = this match {
      case VSuccess(x) => f(x)
      case VError(es) => VError(es)
    }

    def orElse[EE >: E,B >: A](b: => Validation[EE, B]): Validation[EE, B] = this match {
      case VSuccess(x) => VSuccess(x)
      case _ => b
    }

    def map2[EE >: E, B, C](b: Validation[EE, B])(f: (A, B) => C): Validation[EE, C] =
      (this, b) match {
        case (VSuccess(x1), VSuccess(x2)) => VSuccess(f(x1, x2))
        case (VSuccess(_), VError(es)) => VError(es)
        case (VError(es), VSuccess(_)) => VError(es)
        case (VError(es1), VError(es2)) => VError(es1 ++ es2)
      }
  }

  case class VError[+E](errors: List[E]) extends Validation[E, Nothing]
  case class VSuccess[+A](value: A) extends Validation[Nothing, A]
}
