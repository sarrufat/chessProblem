
import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.sarrufat.chesschallenge.Solver

class SolverTest extends FunSuite with Matchers {
  val solver = Solver(3, 3)((2, 'K'), (1, 'R'))
  val solution = solver.solve
  test("Solver must return 4 solutions") {
    solution.length should be(4)
  }
}
