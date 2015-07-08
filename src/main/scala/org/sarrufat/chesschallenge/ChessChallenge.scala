package org.sarrufat.chesschallenge

/**
 * Configuration class
 *
 */
case class Config(dimM: Int = 4, dimN: Int = 4, pieces: Map[String, Int] = Map().empty, printResult: Boolean = false, timing: Boolean = false)
/**
 * Options for parsing args. I'm using scopt, a little command line options parsing library.
 *
 */
object Options {
  private val pTypes = "KQBRN".toList map { _.toString() }

  def apply() = {
    new scopt.OptionParser[Config]("Chess Challenge") {
      head("Chess Challenge", "0.1.0")
      opt[Int]('m', "dimM") required () action ((m, c) ⇒ c.copy(dimM = m)) text ("M board dimension")
      opt[Int]('n', "dimN") required () action ((n, c) ⇒ c.copy(dimN = n)) text ("N board dimension")
      opt[Map[String, Int]]('p', "pieces").
        required() valueName ("k1=v1,k2=v2... where k1..kn= one of 'KQBRN'") action { (p, c) ⇒ c.copy(pieces = p) } validate { p ⇒
          p.keys.forall { k ⇒ pTypes.contains(k) } match {
            case true  ⇒ success
            case false ⇒ failure("invalid piece type")
          }
        }
      opt[Unit]('o', "output") text ("output results") action ((_, c) ⇒ c.copy(printResult = true))
      opt[Unit]('t', "time") text ("timing") action ((_, c) ⇒ c.copy(timing = true))
    }
  }
}
/**
 * The App class
 */
object ChessChallenge extends App {
  Options().parse(args, Config()) match {
    case Some(conf) ⇒
      val solver = Solver2(conf)
      solver.verboseSolve(conf.printResult, conf.timing)
    case None ⇒
  }
}
