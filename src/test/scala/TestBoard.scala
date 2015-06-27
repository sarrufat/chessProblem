
import org.sarrufat.chesschallenge.Board
import org.scalatest.FunSuite
import org.scalatest.Matchers

class TestBoard extends FunSuite with Matchers {

  test("A chess board must have correct dimensions") {
    intercept[AssertionError] {
      Board(0, 0)
      Board(-2, 2)
      Board(2, -2)
    }
  }

  test(" A 3x3 board and King Threatening") {
    val b3x3 = Board(3, 3)
    val ko = b3x3.newPiece('K', (1, 1))
    ko should not be None
    ko.foreach { k ⇒
      k.threatening === List((0, 0), (0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1), (2, 2))
    }
  }
}
