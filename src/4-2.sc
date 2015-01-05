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
