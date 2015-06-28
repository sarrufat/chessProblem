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
        case 'B' ⇒ pieces += pos -> new Bishop(pos, this)
        case 'R' ⇒ pieces += pos -> new Rook(pos, this)
        case 'Q' ⇒ pieces += pos -> new Queen(pos, this)
        case 'N' ⇒ pieces += pos -> new Knight(pos, this)
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

  /**
   * Recursively compute position increments by vector 'incr'
   */
  final protected def vincr(pos: Pos, incr: Pos): Positions = {
    val newPos = (pos._1 + incr._1, pos._2 + incr._2)
    if (board.isInside(newPos))
      newPos +: vincr(newPos, incr)
    else
      List()
  }
}

/**
 * Base of all pieces
 */
abstract class PieceBase(p: Pos, b: Board) extends Piece {
  val pos = p
  val board = b
}

class King(p: Pos, b: Board) extends PieceBase(p, b) {
  def threatening: Positions = {
    val ret = for {
      x ← -1 to 1
      y ← -1 to 1
    } yield (pos._1 + x, pos._2 + y)
    ret filter { p ⇒ p != pos && board.isInside(p) } toList
  }
}

/**
 * Trait with Bishop directions
 */
sealed trait BishopMov extends Piece {
  val northEst: Direction = () ⇒ vincr(pos, (1, -1))
  val southWest: Direction = () ⇒ vincr(pos, (-1, 1))
  val southEst: Direction = () ⇒ vincr(pos, (1, 1))
  val northWest: Direction = () ⇒ vincr(pos, (-1, -1))
}
class Bishop(p: Pos, b: Board) extends PieceBase(p, b) with BishopMov {
  def threatening: Positions = northEst() ++ southEst() ++ southWest() ++ northWest()
}

/**
 * Trait with Rook directions
 */
sealed trait RookMov extends Piece {
  val north: Direction = () ⇒ vincr(pos, (0, -1))
  val south: Direction = () ⇒ vincr(pos, (0, 1))
  val est: Direction = () ⇒ vincr(pos, (1, 0))
  val west: Direction = () ⇒ vincr(pos, (-1, 0))
}

class Rook(p: Pos, b: Board) extends PieceBase(p, b) with RookMov {
  def threatening: Positions = north() ++ est() ++ south() ++ west()
}
class Queen(p: Pos, b: Board) extends PieceBase(p, b) with BishopMov with RookMov {
  def threatening: Positions = north() ++ northEst() ++ est() ++ southEst() ++ south() ++ southWest() ++ west() ++ northWest()
}

object Knight {
  private lazy val movVectors = List((1, -2), (2, -1), (2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1), (-1, -2))
}
class Knight(p: Pos, b: Board) extends PieceBase(p, b) {
  def threatening: Positions = {
    val ret = for { vec ← Knight.movVectors } yield { (pos._1 + vec._1, pos._2 + vec._2) }
    ret filter { p ⇒ board.isInside(p) } toList
  }
}
