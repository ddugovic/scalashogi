package shogi
package format
package psn

import cats.data.Validated
import cats.syntax.option._

case class ParsedPsn(
    initialPosition: InitialPosition,
    tags: Tags,
    sans: Variation
)

case class Variation(value: List[San]) extends AnyVal

object Variation {
  val empty = Variation(Nil)
}

// Standard Algebraic Notation
sealed trait San {

  def apply(situation: Situation): Validated[String, shogi.Move]

  def metas: Metas

  def withMetas(m: Metas): San

  def withComments(s: List[String]): San = withMetas(metas withComments s)

  def withVariations(s: List[Variation]): San = withMetas(metas withVariations s)

  def mergeGlyphs(glyphs: Glyphs): San =
    withMetas(
      metas.withGlyphs(metas.glyphs merge glyphs)
    )
}

case class Move(
    dest: Pos,
    role: Role,
    capture: Boolean = false,
    file: Option[Int] = None,
    rank: Option[Int] = None,
    promotion: Boolean = false,
    metas: Metas = Metas.empty
) extends San {

  def apply(situation: Situation) = move(situation)

  def withMetas(m: Metas) = copy(metas = m)

  def move(situation: Situation): Validated[String, PieceMove] =
    situation.board.pieces.foldLeft(none[PieceMove]) {
      case (None, (pos, piece))
          if piece.color == situation.color && piece.role == role && compare(
            file,
            pos.file.index + 1
          ) && compare(
            rank,
            pos.rank.index + 1
          ) && piece.eyes(pos, dest) =>
        situation moveActorAt pos map { a =>
          (if (promotion) a.promotionMoves(situation) else a.unpromotionMoves(situation)).filter {
            _.dest == dest
          }.head
        }
      case (m, _) => m
    } match {
      case None       => Validated invalid s"No move found: $this\n$situation"
      case Some(move) => Validated valid move
    }

  private def compare[A](a: Option[A], b: A) = a.fold(true)(b ==)
}

case class Drop(
    role: Role,
    dest: Pos,
    metas: Metas = Metas.empty
) extends San {

  def apply(situation: Situation) = drop(situation)

  def withMetas(m: Metas) = copy(metas = m)

  def drop(situation: Situation): Validated[String, PieceDrop] = {
    val piece = Piece(situation.color, role)
    if (situation dropDestsOf piece contains dest)
      Validated valid PieceDrop(piece, dest)
    else Validated invalid s"No $role drop at $dest found: $this\n$situation"
  }
}

case class InitialPosition(
    comments: List[String]
)

case class Metas(
    comments: List[String],
    glyphs: Glyphs,
    variations: List[Variation]
) {

  def withGlyphs(g: Glyphs) = copy(glyphs = g)

  def withComments(c: List[String]) = copy(comments = c)

  def withVariations(v: List[Variation]) = copy(variations = v)
}

object Metas {
  val empty = Metas(Nil, Glyphs.empty, Nil)
}

case class Suffixes(
    promotion: Boolean,
    glyphs: Glyphs
)
