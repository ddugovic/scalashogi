package shogi
package format
package psn

case class Psn(
    tags: Tags,
    moves: NotationMoves,
    initial: Initial = Initial.empty
) extends Notation {

  def withMoves(moves: NotationMoves) =
    copy(moves = moves)

  def withTags(tags: Tags) =
    copy(tags = tags)

  def render: String = {
    val tagsStr = tags.toPSNString
    val initStr =
      if (initial.comments.nonEmpty) initial.comments.mkString("{ ", " } { ", " }\n")
      else ""
    val turnStr   = moves map Psn.renderNotationMove mkString " "
    val resultStr = tags(_.Result) | ""
    val endStr =
      if (turnStr.nonEmpty) s" $resultStr"
      else resultStr
    s"$tagsStr\n\n$initStr$turnStr$endStr"
  }.trim

  override def toString = render
}

object Psn {

  def renderNotationMove(cur: NotationMove) = {
    val number = cur.moveNumber
    val san    = cur.move
    val glyphStr = cur.glyphs.toList.map {
      case glyph if glyph.id <= 6 => glyph.symbol
      case glyph                  => s" $$${glyph.id}"
    }.mkString
    val commentsOrResult =
      if (cur.comments.nonEmpty || cur.result.isDefined)
        List(cur.result).flatten
          .:::(cur.comments map Psn.noDoubleLineBreak)
          .map { text =>
            s" { $text }"
          }
          .mkString
      else ""
    val variationString =
      if (cur.variations.isEmpty) ""
      else cur.variations.map(_.mkString(" (", " ", ")")).mkString(" ")
    s"$number. $san$glyphStr$commentsOrResult$variationString"
  }

  private val noDoubleLineBreakRegex = "(\r?\n){2,}".r

  private def noDoubleLineBreak(txt: String) =
    noDoubleLineBreakRegex.replaceAllIn(txt, "\n")
}
