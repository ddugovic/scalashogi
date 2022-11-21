import ornicar.scalalib

import cats.data.NonEmptyList

package object shogi extends scalalib.Common with scalalib.OrnicarOption with scalalib.OrnicarBoolean {

  val Sente = Color.Sente
  val Gote  = Color.Gote

  type Direction  = Pos => Option[Pos]
  type Directions = List[Direction]

  type PieceMap = Map[Pos, Piece]
  type HandsMap = Map[Piece, Int]
  type HandMap  = Map[Role, Int]

  type PositionHash = Array[Byte]

  type NotationMoves = Vector[format.NotationMove]
  type Moves         = Vector[Move]
  type ParsedMoves   = List[format.ParsedMove]
  type Usis          = Vector[format.usi.Usi]
  implicit def toMoves(moves: NonEmptyList[Move]): Moves        = moves.toVector
  implicit def toUsis(moves: Moves): Usis                       = moves.map(_.usi)
  implicit def toUsis(usis: NonEmptyList[format.usi.Usi]): Usis = usis.toVector
}
