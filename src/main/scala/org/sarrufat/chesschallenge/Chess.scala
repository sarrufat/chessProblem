package org.sarrufat.chesschallenge

/**
 *  This class represents the board's game
 */

case class Board(M: Int, N: Int) {

  assert(M > 0, "Board dimension must be a positive integer")
  assert(N > 0, "Board dimension must be a positive integer")
  // Pieces on board by position
  var pieces = Map[Pos, Piece]().empty
  def isInside(pos: Pos) = pos._1 >= 0 && pos._1 < M && pos._2 >= 0 && pos._2 < N
  def checkFreePos(pos: Pos) = pieces.get(pos) == None
  /**
   * Creates a new piece on position if possible
   * pt represents the type (K, Q, B R, N)
   */
  def newPiece(pt: Char, pos: Pos): Option[Piece] = {
    if (checkFreePos(pos)) {
      pt match {
        case 'K' ⇒ pieces += pos -> new King(pos, this)
      }
    }
    pieces.get(pos)
  }
}
/*
 * A generic piece
 */
sealed trait Piece {
  val pos: Pos
  val board: Board
  /*
   * method for getting the threatening positions on the board
   */
  def threatening: Positions
}

class King(p: Pos, b: Board) extends Piece {
  val pos = p
  val board = b
  def threatening: Positions = {
    val ret = for {
      x ← -1 to 1
      y ← -1 to 1
    } yield (pos._1 + x, pos._2 + y)
    ret filter { p ⇒ p != pos && board.isInside(p) } toList
  }
}
//class Queen extends Piece
//class Bishop extends Piece
//class Rook extends Piece
//class Knight extends Piece
