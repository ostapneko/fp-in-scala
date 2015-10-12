import org.scalatest.FreeSpec

class Chapter4Test extends FreeSpec {
  import Chapter4._

  "4.4" in {
    assertResult(
      Some(List(1,2))
    )(
      sequence(List(Some(1), Some(2)))
    )

    assertResult(
      None
    )(
      sequence(List(Some(1), None))
    )
  }

  "4.5" in {
    assertResult(
      Some(List(1, 2))
    )(
      traverse(List(1, 2))(Some(_))
    )

    assertResult(
      None
    )(
      traverse(List(1, 2)) { i => if (i < 2) Some(i) else None }
    )
  }
}
