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

// https://stackoverflow.com/a/39072117
case class Usis(val underlying: List[Usi]) {
  def toList: List[Usi]     = underlying
  def toSeq: Seq[Usi]       = underlying.toSeq
  def toVector: Vector[Usi] = underlying.toVector
}

object Usis {
  implicit def apply(usis: List[Usi]): Usis   = new Usis(usis)
  implicit def apply(usis: Seq[Usi]): Usis    = new Usis(usis.toList)
  implicit def apply(usis: Vector[Usi]): Usis = new Usis(usis.toList)
  implicit def toList(usis: Usis): List[Usi]  = usis.toList
}

object Usi {

  // https://stackoverflow.com/a/39072117
  case class Moves(val underlying: Vector[shogi.Move]) {
    def toList: List[shogi.Move]     = underlying.toList
    def toSeq: Seq[shogi.Move]       = underlying.toSeq
    def toVector: Vector[shogi.Move] = underlying
    def toUsis: Usis                 = Usis(underlying map toUsi)
    def toUsiList: List[Usi]         = toUsis.toList
  }
  implicit def apply(moves: List[shogi.Move]): Moves      = new Moves(moves.toVector)
  implicit def apply(moves: Seq[shogi.Move]): Moves       = new Moves(moves.toVector)
  implicit def apply(moves: Vector[shogi.Move]): Moves    = new Moves(moves)
  implicit def toUsis(moves: Moves): Usis                 = moves.toUsis
  implicit def toVector(moves: Moves): Vector[shogi.Move] = moves.toVector

  object Moves {

    // TODO: remove backward compatibility code
    def apply(moves: Moves, initialSfen: Option[Sfen], variant: Variant): Moves =
      apply(moves.toUsis, initialSfen, variant)

    def apply(usi: Usi, situation: Situation): Moves = apply(Usis(Vector(usi)), situation)

    def apply(usis: Usis, situation: Situation): Moves = Moves(
      Replay
        .situations(usis, situation)
        .map { _.tail.map { _.history.lastMove.get } }
        .toOption
        .get
        .toVector
    )

    def apply(usis: Usis, variant: Variant): Moves = apply(usis, None, variant)

    def apply(usis: Usis, initialSfen: Option[Sfen], variant: Variant): Moves = Moves(
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

    override def toString: String = usi
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

    override def toString: String = usi
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

  implicit def toString(usi: Usi): String = usi.toString
}
