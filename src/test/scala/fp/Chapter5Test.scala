package fp

import org.scalatest.FreeSpec

class Chapter5Test extends FreeSpec {
  import Chapter5._
  "5.1" in {
    assertResult(List(1, 2))(MyStream(1, 2).toList)
    assertResult(Nil)(MyStream.empty.toList)
  }

  def infiniteStreamFrom(n: Int): MyStream[Int] = MyStream.cons(n, infiniteStreamFrom(n + 1))

  "5.2" in {
    assertResult(List(1, 2))(infiniteStreamFrom(1).take(2).toList)
  }

  "5.3" in {
    assertResult(List(0, 1))(infiniteStreamFrom(0).takeWhile(_ < 2).toList)
  }

  "5.4" in {
    assert(infiniteStreamFrom(0).takeWhile(_ < 2).forAll(_ < 2))
    assert(!infiniteStreamFrom(0).takeWhile(_ < 2).forAll(_ < 1))
  }
}
