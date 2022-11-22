package shogi
package format
package psn

case class ParsedPsn(
    initialPosition: InitialPosition,
    tags: Tags,
    moves: PsnMoves
)

case class PsnMoves(value: List[PsnMove]) extends AnyVal

object PsnMoves {

  val empty = PsnMoves(Nil)
}

// Standard Algebraic Notation
sealed trait PsnMove {

  def metas: Metas

  def withMetas(m: Metas): PsnMove

  def withComments(s: List[String]): PsnMove = withMetas(metas withComments s)

  def withVariations(s: List[PsnMoves]): PsnMove = withMetas(metas withVariations s)

  def mergeGlyphs(glyphs: Glyphs): PsnMove =
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
) extends PsnMove {

  def withMetas(m: Metas) = copy(metas = m)
}

case class Drop(
    role: Role,
    dest: Pos,
    metas: Metas = Metas.empty
) extends PsnMove {

  def withMetas(m: Metas) = copy(metas = m)
}

case class InitialPosition(
    comments: List[String]
)

case class Metas(
    comments: List[String],
    glyphs: Glyphs,
    variations: List[PsnMoves]
) {

  def withGlyphs(g: Glyphs) = copy(glyphs = g)

  def withComments(c: List[String]) = copy(comments = c)

  def withVariations(v: List[PsnMoves]) = copy(variations = v)
}

object Metas {

  val empty = Metas(Nil, Glyphs.empty, Nil)
}

case class Suffixes(
    promotion: Boolean,
    glyphs: Glyphs
)
