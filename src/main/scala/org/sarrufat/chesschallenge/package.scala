package org.sarrufat

package object chesschallenge {

  // Postion on the board
  type Pos = (Int, Int)
  type Positions = Seq[Pos]
  type Direction = () ⇒ Positions
  // types used by solver
  type Dimension = (Int, Int)
  type PieceParam = (Int, Char)
  type ResultPos = (Pos, Char)
  type ResultPositions = List[ResultPos]
  type Results = List[ResultPositions]
  // Type used by solver 2
  type ThreateningVector = Vector[Vector[Pos]]
}
