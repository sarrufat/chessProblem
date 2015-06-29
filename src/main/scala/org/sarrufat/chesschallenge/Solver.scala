package org.sarrufat.chesschallenge

/**
 * The solver class of Chess Challenge
 */
class Solver(dimension: Dimension, pieces: Seq[PieceParam]) {
  val seqPieces = (for {
    p ← pieces
    n ← p._1 to 1 by -1
  } yield p._2).toList

  assert(dimension._1 > 2 && dimension._2 > 2)
  def solve: Results = {
    var results: Results = List[ResultPositions]()
    val targetResLenght = seqPieces.length
    def internalSolver(pieces: List[Char]) = {
      def recSolver(pos: Pos, board: Board, pieces: List[Char]): Unit = {
        val workBoard = Board(board)
        pieces match {
          case ptype :: tail ⇒ {
            workBoard.tryNewPiece(ptype, pos) match {
              case Some(piece) ⇒ recSolver((0, 0), workBoard, tail)
              case None        ⇒
            }
          }
          case Nil ⇒ if (workBoard.isSolved(targetResLenght)) results = results :+ workBoard.toResult
        }
        workBoard.getNextPossiblePosFromPos(pos) match {
          case Some(p) ⇒ recSolver(p, board, pieces)
          case None    ⇒
        }
      }
      val board = Board(dimension._1, dimension._2)
      recSolver((0, 0), board, pieces)
    }
    internalSolver(seqPieces)
    results
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
  def apply(dim: Dimension)(pieces: PieceParam*) = new Solver(dim, pieces)
}
