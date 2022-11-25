import ornicar.scalalib

import cats.data.NonEmptyList

import shogi.format.{ NotationMove, ParsedMove }
import shogi.format.usi.Usi

package object shogi extends scalalib.Common with scalalib.OrnicarOption with scalalib.OrnicarBoolean {

  val Sente = Color.Sente
  val Gote  = Color.Gote

  type Direction  = Pos => Option[Pos]
  type Directions = List[Direction]

  type PieceMap = Map[Pos, Piece]
  type HandsMap = Map[Piece, Int]
  type HandMap  = Map[Role, Int]

  type PositionHash = Array[Byte]

  type NotationMoves = List[NotationMove]
  object NotationMoves {
    def empty = List.empty
  }
  implicit def toNotationMoves(notationMoves: Vector[NotationMove]) = notationMoves.toList

  type Moves       = Vector[Move]
  type ParsedMoves = List[ParsedMove]
  type Usis        = Vector[Usi]
  implicit def toMoves(moves: NonEmptyList[Move]): Moves          = moves.toVector
  implicit def toUsi(move: Move): Usi                             = move.usi
  implicit def toUsiOption(moveOption: Option[Move]): Option[Usi] = moveOption.map(_.usi)
  implicit def toUsis(moves: Moves): Usis                         = moves.map(_.usi)
  implicit def toUsis(usis: List[Usi]): Usis                      = usis.toVector
  implicit def toUsis(usis: NonEmptyList[Usi]): Usis              = usis.toVector
}
