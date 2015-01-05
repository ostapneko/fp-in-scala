def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  for {
    x1 <- a
    x2 <- b
  } yield f(x1, x2)
}

val res = map2(Some("blu"), Some("bla"))((a: String, b : String) => a + b)
