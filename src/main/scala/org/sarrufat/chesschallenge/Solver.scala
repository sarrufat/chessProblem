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
        board.getNextPossiblePosFromPos(pos) match {
          case Some(p) ⇒ recSolver(p, board, pieces)
          case None    ⇒
        }
      }
      val board = Board(dimension._1, dimension._2)
      recSolver((0, 0), board, pieces)
    }
    internalSolver(seqPieces)
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
    println(s"Trying to solve ${dimension._1}X${dimension._2} board with ${verbosePieces} pieces...")
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
    val pieces = conf.pieces.map(cp ⇒ (cp._2, cp._1.charAt(0))).toSeq
    apply((conf.dimM, conf.dimN))(pieces: _*)
  }
}