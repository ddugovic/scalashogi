package shogi
package format

import shogi.format.usi.Usi

case class ParsedNotation(
    initialPosition: InitialPosition,
    tags: Tags,
    parsedMoves: ParsedMoves
)

sealed trait ParsedMove {

  def usi: Usi

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

// TODO: Remove compatibility facade
object ParsedMove {

  implicit def toUsi(parsedMove: ParsedMove): Usi = parsedMove.usi
}

case class KifMove(
    dest: Pos,
    orig: Pos,
    role: Role,
    promotion: Boolean = false,
    metas: Metas = Metas.empty
) extends ParsedMove {

  def usi: Usi = Usi.Move(orig, dest, promotion)

  def withMetas(m: Metas) = copy(metas = m)
}

case class CsaMove(
    dest: Pos,
    orig: Pos,
    role: Role,
    promotion: Boolean = false,
    metas: Metas = Metas.empty
) extends ParsedMove {

  def usi: Usi = Usi.Move(orig, dest, promotion)

  def withMetas(m: Metas) = copy(metas = m)
}

// All notations can share drop
case class ParsedDrop(
    role: Role,
    dest: Pos,
    metas: Metas = Metas.empty
) extends ParsedMove {

  def usi: Usi = Usi.Drop(role, dest)

  def withMetas(m: Metas) = copy(metas = m)
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
