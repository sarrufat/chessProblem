
import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.sarrufat.chesschallenge.Solver2

class Solver2Test extends FunSuite with Matchers {

  test("S2 3x3 board containing 2K and 1R. Solver must return 4 solutions") {
    val solver = Solver2(3, 3)((2, 'K'), (1, 'R'))
    val solution = solver.solve
    solution.length should be(4)
  }
  test("S2 4x4 board containing 2R and 4N. Solver must return 8 solutions") {
    val solver = Solver2(4, 4)((2, 'R'), (4, 'N'))
    val solution = solver.solve
    solution.length should be(8)
  }

  test("S2 4x4 board containing 4Q. Solver must return 2 solutions") {
    val solver = Solver2(4, 4)((4, 'Q'))
    val solution = solver.solve
    solution.length should be(2)
  }
  test("S2 5x5 board containing 5Q. Solver must return 10 solutions") {
    val solver = Solver2(5, 5)((5, 'Q'))
    val solution = solver.solve
    solution.length should be(10)
  }
  test("S2 6x6 board containing 6Q. Solver must return 4 solutions") {
    val solver = Solver2(6, 6)((6, 'Q'))
    val solution = solver.solve
    solution.length should be(4)
  }
  test("S2 8x8 board containing 8Q. Solver must return 92 solutions") {
    val solver = Solver2(8, 8)((8, 'Q'))
    val solution = solver.solve
    solution.length should be(92)
  }
  test("S2 9x9 board containing 9Q. Solver must return 352 solutions") {
    val solver = Solver2(9, 9)((9, 'Q'))
    val solution = solver.solve
    solution.length should be(352)
  }
}
