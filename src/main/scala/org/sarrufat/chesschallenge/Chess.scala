package org.sarrufat.chesschallenge

object Board {
  /*
   *
   */
  def genCells(M: Int, N: Int) = for {
    x ← 0 until M
    y ← 0 until N
  } yield (x, y)
  /*
   *  Copy factory
   */
  def apply(b: Board): Board = {
    val cb = Board(b.M, b.N)
    cb.pieces = Map(b.pieces.toSeq: _*)
    cb
  }
}
/**
 *  This class represents the board's game
 */

case class Board(M: Int, N: Int) {
  assert(M > 0, "Board dimension must be a positive integer")
  assert(N > 0, "Board dimension must be a positive integer")
  private lazy val allPos = Board.genCells(M, N)
  // Pieces on board by position
  var pieces = Map[Pos, Piece]().empty
  def isInside(pos: Pos) = pos._1 >= 0 && pos._1 < M && pos._2 >= 0 && pos._2 < N
  def checkFreePos(pos: Pos) = pieces.get(pos) == None && !nonFree.contains(pos)
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
        case _   ⇒ throw new Exception(s"Unknown piece type '$pt'")
      }
      pieces.get(pos)
    } else
      None
  }
  def removePiece(piece: Piece) = pieces = pieces - piece.pos
  def nonFree = (pieces map { p ⇒ (p._1 +: p._2.threatening) }).flatten.toSeq
  def getPossibleCells = {
    // nonFree = occopied + threatening
    Board.genCells(M, N).filterNot(p ⇒ nonFree contains (p)).sortBy(p ⇒ p).toList
  }
  def getNextPossiblePosFromPos(pos: Pos): Option[Pos] = {
    val nextList = getPossibleCells
    nextList.find { p ⇒ (p._1 * M + p._2) > (pos._1 * M + pos._2) }

  }
  /**
   * Tries to Create a new piece on position if possible and no threatening the other pieces on the board
   */
  def tryNewPiece(pt: Char, pos: Pos): Option[Piece] = {
    val np = newPiece(pt, pos)
    np match {
      case Some(curPiece) ⇒ if (curPiece.threatening exists { p ⇒ pieces contains (p) }) {
        removePiece(curPiece)
        None
      } else
        np
      case None ⇒ None
    }
  }
  def isSolved(ntarg: Int) = pieces.size == ntarg
  def toResult = (pieces.map { piece ⇒ (piece._1, piece._2.toChar) }).toList.sortBy(p ⇒ p._1)
  override def toString = {
    val result = toResult
    var retStr = ""
    for (res ← result) {
      retStr += s"${res} "
    }
    retStr
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
    if (board isInside (newPos))
      newPos +: vincr(newPos, incr)
    else
      List()
  }
  def toChar: Char
}

/**
 * Base of all pieces
 */
abstract class PieceBase(p: Pos, b: Board) extends Piece {
  val pos = p
  val board = b
}

class King(p: Pos, b: Board) extends PieceBase(p, b) {
  private lazy val _threatening = {
    val ret = for {
      x ← -1 to 1
      y ← -1 to 1
    } yield (pos._1 + x, pos._2 + y)
    ret filter { p ⇒ p != pos && board.isInside(p) } toList
  }
  def threatening: Positions = _threatening
  def toChar: Char = 'K'
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
  private lazy val _threatening = northEst() ++ southEst() ++ southWest() ++ northWest()
  def threatening: Positions = _threatening
  def toChar: Char = 'B'
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
  private lazy val _threatening = north() ++ est() ++ south() ++ west()
  def threatening: Positions = _threatening
  def toChar: Char = 'R'
}
class Queen(p: Pos, b: Board) extends PieceBase(p, b) with BishopMov with RookMov {
  private lazy val _threatening = north() ++ northEst() ++ est() ++ southEst() ++ south() ++ southWest() ++ west() ++ northWest()
  def threatening: Positions = _threatening
  def toChar: Char = 'Q'
}

object Knight {
  private lazy val movVectors = List((1, -2), (2, -1), (2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1), (-1, -2))
}
class Knight(p: Pos, b: Board) extends PieceBase(p, b) {
  private lazy val _threatening = {
    val ret = for { vec ← Knight.movVectors } yield { (pos._1 + vec._1, pos._2 + vec._2) }
    ret filter { p ⇒ board isInside (p) } toList
  }
  def threatening: Positions = _threatening
  def toChar: Char = 'N'
}
