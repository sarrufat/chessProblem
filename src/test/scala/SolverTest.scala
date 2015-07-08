
import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.sarrufat.chesschallenge.Solver

class SolverTest extends FunSuite with Matchers {

  test("3x3 board containing 2K and 1R. Solver must return 4 solutions") {
    val solver = Solver(3, 3)((2, 'K'), (1, 'R'))
    val solution = solver.solve
    solution.length should be(4)
  }
  test("4x4 board containing 2R and 4N. Solver must return 8 solutions") {
    val solver = Solver(4, 4)((2, 'R'), (4, 'N'))
    val solution = solver.solve
    solution.length should be(8)
  }

  test("4x4 board containing 4Q. Solver must return 2 solutions") {
    val solver = Solver(4, 4)((4, 'Q'))
    val solution = solver.solve
    solution.length should be(2)
  }
  test("5x5 board containing 5Q. Solver must return 10 solutions") {
    val solver = Solver(5, 5)((5, 'Q'))
    val solution = solver.solve
    solution.length should be(10)
  }
  test("6x6 board containing 6Q. Solver must return 4 solutions") {
    val solver = Solver(6, 6)((6, 'Q'))
    val solution = solver.solve
    solution.length should be(4)
  }
}
