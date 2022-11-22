package shogi
package format.usi

import cats.implicits._

sealed trait Usi {

  def usi: String
  def uci: String // will be removed

  def dest: Pos
}

object Usi {

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

    override def toString: String = usi
  }

  object Move {

    def apply(move: String): Option[Move] =
      for {
        orig <- Pos.fromKey(move take 2)
        dest <- Pos.fromKey(move.slice(2, 4))
      } yield Move(orig, dest, move.takeRight(1) == "+")

  }

  case class Drop(role: Role, dest: Pos) extends Usi {

    def usi = s"${role.forsythUpper}*${dest.usiKey}"

    def uci = s"${role.forsythUpper}*${dest.uciKey}"

    override def toString: String = usi
  }

  object Drop {

    def apply(drop: String): Option[Drop] =
      for {
        role <- Role.allByForsythUpper.get(drop.takeWhile(_ != '*'))
        dest <- Pos.fromKey(drop takeRight 2)
      } yield Drop(role, dest)

  }

  def apply(usiStr: String): Option[Usi] =
    if (usiStr contains '*')
      Usi.Drop(usiStr)
    else Usi.Move(usiStr)

  def readMoves(moves: List[String]): Option[Usis] =
    moves.toVector.map(apply).sequence

  def readMoves(moves: String): Option[Usis] =
    readMoves(moves.split(' ').toList)

  def readList(moves: String): Option[List[Usi]] =
    readMoves(moves).map(_.toList)

  implicit def toString(usi: Usi): String = usi.toString
}
