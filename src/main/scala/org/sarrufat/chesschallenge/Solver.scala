package org.sarrufat.chesschallenge

import scala.annotation.tailrec

/**
 * The solver class of Chess Challenge
 */
class Solver(dimension: Dimension, pieces: Seq[PieceParam]) {

  val seqPieces = (for {
    p ← pieces
    n ← p._1 to 1 by -1
  } yield p._2).toList

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
  /**
   * This algorithm solver is a recursive algorithm with backtracking.
   * @return
   */
  def solve: Results = {
    var results: Results = List[ResultPositions]()
    val targetResLenght = seqPieces.length
    def internalSolver(pieces: List[Char]) = {
      val board = Board(dimension._1, dimension._2)
      def comparePos(p1: Pos, p2: Pos) = (p1._1 * dimension._2 + p1._2) - (p2._1 * dimension._2 + p2._2)
      // Recursive Solver
      def recSolver(positions: Positions, pieces: List[Char], parentPos: Seq[ResultPos]): Unit = {
        // Compute next possible (non threatening) positions
        for (pos ← positions) {

          // Copy the actual board
          pieces match {
            // Non empty pieces and skip permutations already processed
            case ptype :: tail if (parentPos.isEmpty || parentPos.head._2 != ptype || comparePos(pos, parentPos.head._1) > 0) ⇒ {
              // try to create a new piece on this position
              board.tryNewPiece(ptype, pos) match {
                // Success: then compute the next deeper level of the tree if more pieces to place on the board
                case Some(piece) ⇒
                  if (board.isSolved(targetResLenght))
                    results = results :+ board.toResult
                  else
                    recSolver(board.getPossibleCells, tail, parentPos :+ (pos, ptype))
                  // Remove pieces -- shared array between probes
                  board.removePiece(piece)
                // Not possible
                case None ⇒
              }
            }
            // No more levels: if solved add to results
            case _ ⇒
          }
        }
      }
      recSolver(board.getPossibleCells, pieces, Seq())
      board.tryCounter
    }
    val tc = internalSolver(seqPieces)
    //    println("tryCounter = " + tc)
    // remove duplicates
    val resMap = results.groupBy { _.toString }
    val res = for (m ← resMap) yield m._2.head
    res.toList
  }
  def verboseSolve(print: Boolean) = {
    def verbosePieces = {
      val names = Map('K' -> "Kings", 'Q' -> "Queens", 'B' -> "Bishops", 'R' -> "Rooks", 'N' -> "Knights")
      pieces.map { p ⇒ p._1 + s" ${names.get(p._2).get} " } mkString (" and ")
    }
    println(s"Trying to solve ${dimension._1}X${dimension._2} board with ${verbosePieces} ...")
    val results = solve
    println(s"Found ${results.length} solutions")
    if (print) results.foreach { printresult(_) }
  }
}
/**
 * The solver factory object
 */
object Solver {
  /**
   * Creates a solver
   * @param dim dimensions of the board
   * @param pieces
   * @return
   */
  def apply(dim: Dimension)(pieces: PieceParam*): Solver = new Solver(dim, pieces)

  /**
   * Creates a solver from configuration
   * @param conf
   * @return
   */
  def apply(conf: Config): Solver = {
    val pieces = conf.pieces.toSeq.map(cp ⇒ (cp._2, cp._1.charAt(0))).toSeq
    apply((conf.dimM, conf.dimN))(pieces: _*)
  }
}
