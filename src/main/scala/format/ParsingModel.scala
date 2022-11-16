package shogi
package format

import cats.data.Validated
import cats.data.Validated.{ invalid, valid }

case class ParsedNotation(
    initialPosition: InitialPosition,
    tags: Tags,
    parsedMoves: ParsedMoves
)

// TODO: rename value -> underlying to follow common convention
case class ParsedMoves(value: List[ParsedMove]) {
  def toList: List[ParsedMove] = value
}

object ParsedMoves {
  val empty                                                       = ParsedMoves(List.empty)
  def apply(value: List[ParsedMove]): ParsedMoves                 = new ParsedMoves(value)
  implicit def toList(parsedMoves: ParsedMoves): List[ParsedMove] = parsedMoves.toList
}

sealed trait ParsedMove {

  def toMove(sit: Situation): Validated[String, Move]

  def positions: List[Pos]

  def metas: Metas

  def withMetas(m: Metas): ParsedMove

  def withComments(s: List[String]): ParsedMove = withMetas(metas withComments s)

  def withVariations(s: List[ParsedMoves]): ParsedMove = withMetas(metas withVariations s)

  def withTimeSpent(ts: Option[Centis]): ParsedMove = withMetas(metas withTimeSpent ts)

  def withTimeTotal(tt: Option[Centis]): ParsedMove = withMetas(metas withTimeTotal tt)

  def mergeGlyphs(glyphs: Glyphs): ParsedMove =
    withMetas(
      metas.withGlyphs(metas.glyphs merge glyphs)
    )

}

case class KifMove(
    dest: Pos,
    orig: Pos,
    role: Role,
    promotion: Boolean = false,
    metas: Metas = Metas.empty
) extends ParsedMove {

  def toMove(sit: Situation): Validated[String, Move] =
    Validated.fromOption(sit.board(orig), s"No piece at $orig") map { p =>
      PieceMove(sit.board, p, orig, dest, promotion)
    }

  def withMetas(m: Metas) = copy(metas = m)

  def positions = List(orig, dest)

}

case class CsaMove(
    dest: Pos,
    orig: Pos,
    role: Role,
    metas: Metas = Metas.empty
) extends ParsedMove {

  def toMove(sit: Situation): Validated[String, Move] =
    Validated.fromOption(sit.board(orig), s"No piece at $orig") map { p =>
      PieceMove(sit.board, p, orig, dest, role != p.role)
    }

  def withMetas(m: Metas) = copy(metas = m)

  def positions = List(orig, dest)

}

// All notations can share drop
case class ParsedDrop(
    role: Role,
    pos: Pos,
    metas: Metas = Metas.empty
) extends ParsedMove {

  def toMove(sit: Situation): Validated[String, Move] =
    if (sit.variant.handRoles contains role) valid(PieceDrop(Piece(sit.color, role), pos))
    else invalid(s"$role can't be dropped in ${sit.variant} shogi")

  def withMetas(m: Metas) = copy(metas = m)

  def positions = List(pos)
}

case class InitialPosition(
    comments: List[String]
)

case class Metas(
    comments: List[String],
    glyphs: Glyphs,
    variations: List[ParsedMoves],
    timeSpent: Option[Centis],
    timeTotal: Option[Centis]
) {

  def withGlyphs(g: Glyphs) = copy(glyphs = g)

  def withComments(c: List[String]) = copy(comments = c)

  def withVariations(v: List[ParsedMoves]) = copy(variations = v)

  def withTimeSpent(ts: Option[Centis]) = copy(timeSpent = ts)

  def withTimeTotal(tt: Option[Centis]) = copy(timeTotal = tt)
}

object Metas {
  val empty = Metas(Nil, Glyphs.empty, Nil, None, None)
}

case class Suffixes(
    promotion: Boolean,
    glyphs: Glyphs
)
