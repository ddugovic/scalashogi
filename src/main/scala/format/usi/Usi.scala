package shogi
package format
package usi

import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._
import shogi.format.forsyth.Sfen
import shogi.variant.Variant

sealed trait Usi {

  def usi: String
  def uci: String // will be removed

  def positions: List[Pos]
  def toMove(sit: Situation): Validated[String, shogi.Move]

}

object Usi {

  // https://stackoverflow.com/a/39072117
  case class Moves(val underlying: Vector[shogi.Move]) {
    def toList: List[shogi.Move]     = underlying.toList
    def toSeq: Seq[shogi.Move]       = underlying.toSeq
    def toVector: Vector[shogi.Move] = underlying
    // TODO: remove Usi facade
    def toUsis: Usis = toList.map(_.usi)
  }
  implicit def apply(moves: Vector[shogi.Move]): Moves    = new Moves(moves)
  implicit def toVector(moves: Moves): Vector[shogi.Move] = moves.toVector

  object Moves {

    // TODO: remove backward compatibility code
    def apply(usis: List[Usi], variant: Variant): Moves = apply(usis, None, variant)

    def apply(usis: Vector[Usi], variant: Variant): Moves = apply(usis.toList, None, variant)

    def apply(usis: List[Usi], initialSfen: Option[Sfen], variant: Variant): Moves = Moves(
      Replay
        .situations(usis, initialSfen, variant)
        .map { _.tail.map { _.history.lastMove.get } }
        .toOption
        .get
        .toVector
    )

    def apply(usis: Vector[Usi], initialSfen: Option[Sfen], variant: Variant): Moves =
      apply(usis.toList, initialSfen, variant)
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

    def toMove(sit: Situation): Validated[String, shogi.Move] =
      Validated.fromOption(sit.board(orig), s"No piece at $orig") map { p =>
        PieceMove(sit.board, p, orig, dest, promotion)
      }

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

    def toMove(sit: Situation): Validated[String, shogi.Move] =
      if (sit.variant.handRoles contains role) valid(PieceDrop(Piece(sit.color, role), pos))
      else invalid(s"$role can't be dropped in ${sit.variant} shogi")

    override def toString: String = usi
  }

  object Drop {

    def apply(drop: String): Option[Drop] =
      for {
        role <- Role.allByForsythUpper.get(drop.takeWhile(_ != '*'))
        pos  <- Pos.fromKey(drop takeRight 2)
      } yield Drop(role, pos)

  }

  def apply(usiStr: String): Option[Usi] =
    if (usiStr contains '*')
      Usi.Drop(usiStr)
    else Usi.Move(usiStr)

  def readList(moves: List[String]): Option[Usis] =
    moves.toList.map(apply).sequence

  def readList(moves: String): Option[Usis] =
    readList(moves.split(' ').toList)

  implicit def toString(usi: Usi): String = usi.toString
}
