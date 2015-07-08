
import org.sarrufat.chesschallenge.ChessChallenge
import org.scalatest.{ FunSuite, Matchers }
import org.sarrufat.chesschallenge.Options
import org.sarrufat.chesschallenge.Config

class MainTest extends FunSuite with Matchers {

  test("Options args") {
    val args = "-m 10 -n 9 -p N=4,R=2,Q=1,K=2 -t".split(' ')
    val result = Options().parse(args, Config())
    result shouldBe defined
    val cfg = result.get
    cfg should be(Config(10, 9, Map("N" -> 4, "R" -> 2, "Q" -> 1, "K" -> 2), false, true))
  }

  test("App Testing: -m 1 -n 1 -p N=4,R=2 ") {
    val args = "-m 1 -n 1 -p N=4,R=2".split(' ')
    intercept[AssertionError] { util.Util.stdoutToString { ChessChallenge.main(args) } }
  }
  test("App Testing without args ") {
    val args = "".split(' ')
    val result = util.Util.stderrToString { ChessChallenge.main(args) }
    result should include("-m <value> | --dimM <value>")
    result should include("-n <value> | --dimN <value>")
    result should include("-p k1=v1,k2=v2... where k1..kn=")
    result should include("-o | --output")
    result should include("-t | --time")

  }
  test("App Testing: -m 4 -n 4 -p N=4,R=2 ") {
    val args = "-m 4 -n 4 -p N=4,R=2".split(' ')
    val result = util.Util.stdoutToString { ChessChallenge.main(args) }
    result should startWith("Trying to solve 4X4 board with 4 Knights  and 2 Rooks  ...")
    result should include("Found 8 solutions")
  }
}
