package shogi
package format
package usi

import cats.implicits._
import shogi.format.forsyth.Sfen
import shogi.variant.Variant

sealed trait Usi {

  def usi: String
  def uci: String // will be removed

  def positions: List[Pos]

}

object Usi {

  // https://stackoverflow.com/a/39072117
  case class Moves(val underlying: Vector[shogi.Move]) {
    def toVector: Vector[shogi.Move] = underlying
    def toList: List[shogi.Move]     = underlying.toList
    def usiMoves: Vector[Usi]        = underlying map toUsiMove
    def usiMoveList: List[Usi]       = usiMoves.toList
  }
  implicit def apply(moves: Vector[shogi.Move]): Moves    = new Moves(moves)
  implicit def toVector(moves: Moves): Vector[shogi.Move] = moves.toVector

  object Moves {

    // TODO: remove backward compatibility code
    def apply(moves: Moves, initialSfen: Option[Sfen], variant: Variant): Moves =
      apply(moves.usiMoveList, initialSfen, variant)

    def apply(usi: Usi, situation: Situation): Moves = apply(List(usi), situation)

    def apply(usis: List[Usi], situation: Situation): Moves = Moves(
      Replay
        .situations(usis, situation)
        .map { _.tail.map { _.history.lastMove.get } }
        .toOption
        .get
        .toVector
    )

    def apply(usis: List[Usi], variant: Variant): Moves = apply(usis, None, variant)

    def apply(usis: List[Usi], initialSfen: Option[Sfen], variant: Variant): Moves = Moves(
      Replay
        .situations(usis, initialSfen, variant)
        .map { _.tail.map { _.history.lastMove.get } }
        .toOption
        .get
        .toVector
    )

    def apply(usis: Seq[Usi], variant: Variant): Moves = apply(usis, None, variant)

    def apply(usis: Seq[Usi], initialSfen: Option[Sfen], variant: Variant): Moves = Moves(
      Replay
        .situations(usis, initialSfen, variant)
        .map { _.tail.map { _.history.lastMove.get } }
        .toOption
        .get
        .toVector
    )

    def apply(usis: Vector[Usi], variant: Variant): Moves = apply(usis, None, variant)

    def apply(usis: Vector[Usi], initialSfen: Option[Sfen], variant: Variant): Moves = Moves(
      Replay
        .situations(usis, initialSfen, variant)
        .map { _.tail.map { _.history.lastMove.get } }
        .toOption
        .get
        .toVector
    )
  }

  case class Move(
      orig: Pos,
      dest: Pos,
      promotion: Boolean = false
  ) extends Usi {

    def usiKeys = orig.usiKey + dest.usiKey
    def usi     = usiKeys + promotionString

    def uciKeys = orig.uciKey + dest.uciKey
    def uci     = uciKeys + promotionString

    def promotionString = if (promotion) "+" else ""

    def positions = List(orig, dest)

    def apply(pieces: PieceMap): shogi.Move =
      PieceMove(pieces(orig), orig, dest, pieces.contains(dest), promotion)

    def apply(situation: Situation): shogi.Move = apply(situation.board.pieces)
  }

  object Move {

    def apply(move: String): Option[Move] =
      for {
        orig <- Pos.fromKey(move take 2)
        dest <- Pos.fromKey(move.slice(2, 4))
      } yield Move(orig, dest, move.takeRight(1) == "+")

  }

  case class Drop(role: Role, pos: Pos) extends Usi {

    def usi = s"${role.forsythUpper}*${pos.usiKey}"

    def uci = s"${role.forsythUpper}*${pos.uciKey}"

    def positions = List(pos)

    def apply(color: Color): shogi.Move =
      PieceDrop(Piece(color, role), pos)

    def apply(situation: Situation): shogi.Move = apply(situation.color)
  }

  object Drop {

    def apply(drop: String): Option[Drop] =
      for {
        role <- Role.allByForsythUpper.get(drop.takeWhile(_ != '*'))
        pos  <- Pos.fromKey(drop takeRight 2)
      } yield Drop(role, pos)

  }

  case class WithRole(usi: Usi, role: Role)

  def apply(usiStr: String): Option[Usi] =
    if (usiStr contains '*')
      Usi.Drop(usiStr)
    else Usi.Move(usiStr)

  def readList(moves: List[String]): Option[List[Usi]] =
    moves.toList.map(apply).sequence

  def readList(moves: String): Option[List[Usi]] =
    readList(moves.split(' ').toList)
}
