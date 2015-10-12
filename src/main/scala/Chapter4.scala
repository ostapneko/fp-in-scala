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
}
