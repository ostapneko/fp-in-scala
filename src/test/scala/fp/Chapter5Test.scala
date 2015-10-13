package fp

import org.scalatest.FreeSpec

class Chapter5Test extends FreeSpec {
  import Chapter5._
  "5.1" in {
    assertResult(List(1, 2))(StreamCustom(1, 2).toList)
    assertResult(Nil)(StreamCustom.empty.toList)
  }

  def infiniteStreamFrom(n: Int): StreamCustom[Int] = StreamCustom.cons(n, infiniteStreamFrom(n + 1))

  "5.2" in {
    assertResult(List(1, 2))(infiniteStreamFrom(1).take(2).toList)
  }

  "5.3" in {
    assertResult(List(2, 3))(infiniteStreamFrom(0).drop(2).take(2).toList)
  }
}
