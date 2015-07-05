package org.sarrufat.chesschallenge

import scala.Vector
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION

/**
 * The solver class of Chess Challenge
 */
class Solver2(dimension: Dimension, pieces: Seq[PieceParam]) {

  val seqPieces = (for {
    p ← pieces
    n ← p._1 to 1 by -1
  } yield p._2).toList.mkString

  assert(dimension._1 > 2 && dimension._2 > 2)
  /**
   * Print result as:
   * +-+-+-+-+
   * |*|R|*|*|
   * +-+-+-+-+
   * |N|*|N|*|
   * +-+-+-+-+
   * |*|*|*|R|
   * +-+-+-+-+
   * |N|*|N|*|
   * +-+-+-+-+
   * @param res
   */
  def printresult(res: ResultPositions) {
    println("")
    def printlnsep = {
      val headS = for (x ← 0 until dimension._1) yield "-+"
      println("\n+" + headS.mkString)
    }
    for (y ← 0 until dimension._2) {
      printlnsep
      print('|')
      for (x ← 0 until dimension._1) {
        val pos = (x, y)
        res.find { p ⇒ p._1 == pos } match {
          case Some((_, k)) ⇒ print(k)
          case None         ⇒ print('*')
        }
        print('|')
      }
    }
    printlnsep
    println("")
  }
  private def threateningPT(pt: Char): ThreateningVector = {
    var tv = Vector[Vector[Pos]]()
    for {
      x ← 0 until dimension._1
      y ← 0 until dimension._2
    } {
      //      val idx = y * dimension._1 + x
      val board = new Board(dimension._1, dimension._2)
      board.newPiece(pt, (x, y)) match {
        case Some(p) ⇒ tv = tv :+ Vector[Pos](p.threatening: _*)
        case None    ⇒
      }
    }
    tv
  }
  private lazy val threateningVectors = {
    // For each piece type
    (for (pp ← pieces) yield (pp._2 -> threateningPT(pp._2))).toMap
  }
  /**
   * This algorithm solver is a recursive algorithm with backtracking.
   * @return
   */
  def solve: Results = {
    var results: Results = List[ResultPositions]()
    def recResul(keys: String, thr: Vector[Pos], resPos: ResultPositions): Unit = {

      for {
        x ← 0 until dimension._1
        y ← 0 until dimension._2
        if (!resPos.map(_._1).contains((x, y)) && !thr.contains((x, y)))
      } {
        val k = keys(0)
        val idx = x * dimension._2 + y
        val lastPos = resPos.last
        val idxlp = lastPos._1._1 * dimension._2 + lastPos._1._2

        if (lastPos._2 != k || idx > idxlp) {
          val thrK = threateningVectors.get(k).get(idx)
          // Verify bno threatenin
          val currTree = resPos.map(_._1)
          if (!thrK.exists(currTree.contains(_))) {
            if (keys.length() == 1) {
              //              println(resPos :+ ((x, y), keys(0)))
              results = results :+ ((resPos :+ ((x, y), keys(0))).sortBy { _._1 })
            } else
              recResul(keys.drop(1), thr ++ thrK, resPos ++ List(((x, y), k)))
          }
        }
      }
    }
    val k = seqPieces(0)
    val thrK = threateningVectors.get(k).get
    // The roots of trees
    for {
      x ← 0 until dimension._1
      y ← 0 until dimension._2
    } {
      val idx = x * dimension._2 + y
      recResul(seqPieces.drop(1), thrK(idx), List(((x, y), k)))
    }
    val resMap = results.groupBy { _.mkString }
    val res = for (m ← resMap) yield m._2.head
    res.toList
  }
  def verboseSolve(print: Boolean, timing: Boolean) = {
    def verbosePieces = {
      val names = Map('K' -> "Kings", 'Q' -> "Queens", 'B' -> "Bishops", 'R' -> "Rooks", 'N' -> "Knights")
      pieces.map { p ⇒ p._1 + s" ${names.get(p._2).get} " } mkString (" and ")
    }
    println(s"Trying to solve ${dimension._1}X${dimension._2} board with ${verbosePieces} ...")
    val t0 = System.currentTimeMillis();
    val results = solve
    val t1 = System.currentTimeMillis();
    if (timing)
      println(s"Found ${results.length} solutions in " + (t1 - t0).toDouble / 1000.0 + " secs.")
    else
      println(s"Found ${results.length} solutions")

    if (print) results.foreach { printresult(_) }
  }
}
/**
 * The solver factory object
 */
object Solver2 {
  /**
   * Creates a solver
   * @param dim dimensions of the board
   * @param pieces
   * @return
   */
  def apply(dim: Dimension)(pieces: PieceParam*): Solver2 = new Solver2(dim, pieces)

  /**
   * Creates a solver from configuration
   * @param conf
   * @return
   */
  def apply(conf: Config): Solver2 = {
    val pieces = conf.pieces.toSeq.map(cp ⇒ (cp._2, cp._1.charAt(0))).toSeq
    apply((conf.dimM, conf.dimN))(pieces: _*)
  }
}
