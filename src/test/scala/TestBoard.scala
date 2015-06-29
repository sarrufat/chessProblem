
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
  test("try") {
    val b4x4 = Board(4, 4)
    b4x4.newPiece('R', (0, 1))
    b4x4.newPiece('R', (1, 0))
    assert(b4x4.tryNewPiece('N', (0, 0)) == None)
  }

  test(" A 3x3 board and King on (0,0) Threatening") {
    val b3x3 = Board(3, 3)
    val ko = b3x3.newPiece('K', (0, 0))
    ko should not be None
    ko.foreach { k ⇒
      assert(k.threatening === List((0, 1), (1, 0), (1, 1)))
    }
  }
  test(" A 3x3 board and King on (1,1) Threatening") {
    val b3x3 = Board(3, 3)
    val ko = b3x3.newPiece('K', (1, 1))
    ko should not be None
    ko.foreach { k ⇒
      assert(k.threatening === List((0, 0), (0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1), (2, 2)))
    }
  }
  test(" A 3x3 board and King on (2,1) Threatening") {
    val b3x3 = Board(3, 3)
    val ko = b3x3.newPiece('K', (2, 1))
    ko should not be None
    ko.foreach { k ⇒
      assert(k.threatening === List((1, 0), (1, 1), (1, 2), (2, 0), (2, 2)))
    }
  }
  test(" A 5x5 board and Bishop on (2,2) Threatening") {
    val b3x3 = Board(5, 5)
    val ko = b3x3.newPiece('B', (2, 2))
    ko should not be None
    ko.foreach { k ⇒
      val res = k.threatening
      assert(res === List((3, 1), (4, 0), (3, 3), (4, 4), (1, 3), (0, 4), (1, 1), (0, 0)))
    }
  }
  test(" A 5x5 board and Rook on (2,2) Threatening") {
    val b3x3 = Board(5, 5)
    val ko = b3x3.newPiece('R', (2, 2))
    ko should not be None
    ko.foreach { k ⇒
      val res = k.threatening
      assert(res === List((2, 1), (2, 0), (3, 2), (4, 2), (2, 3), (2, 4), (1, 2), (0, 2)))
    }
  }
  test(" A 5x5 board and Qeen on (2,2) Threatening") {
    val b3x3 = Board(5, 5)
    val ko = b3x3.newPiece('Q', (2, 2))
    ko should not be None
    ko.foreach { k ⇒
      val res = k.threatening
      assert(res === List((2, 1), (2, 0), (3, 1), (4, 0), (3, 2), (4, 2), (3, 3), (4, 4), (2, 3), (2, 4), (1, 3), (0, 4), (1, 2), (0, 2), (1, 1), (0, 0)))
    }
  }
  test(" A 5x5 board and Knight on (2,2) Threatening") {
    val b3x3 = Board(5, 5)
    val ko = b3x3.newPiece('N', (2, 2))
    ko should not be None
    ko.foreach { k ⇒
      val res = k.threatening
      assert(res === List((3, 0), (4, 1), (4, 3), (3, 4), (1, 4), (0, 3), (0, 1), (1, 0)))
    }
  }
}
