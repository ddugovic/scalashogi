package shogi
package format
package psn

case class Psn(
    tags: Tags,
    turns: List[Turn],
    initial: Initial = Initial.empty
) {

  def updateTurn(ply: Int, f: Turn => Turn) = {
    val index = ply - 1
    (turns lift index).fold(this) { turn =>
      copy(turns = turns.updated(index, f(turn)))
    }
  }
  def updateLastPly(f: Turn => Turn) = updateTurn(turns.size, f)

  def withEvent(title: String) =
    copy(
      tags = tags + Tag(_.Event, title)
    )

  override def toString: String = {
    val tagsStr = tags.toPSNString
    val initStr =
      if (initial.comments.nonEmpty) initial.comments.mkString("{ ", " } { ", " }\n")
      else ""
    val turnStr   = turns mkString " "
    val resultStr = tags(_.Result) | ""
    val endStr =
      if (turnStr.nonEmpty) s" $resultStr"
      else resultStr
    s"$tagsStr\n\n$initStr$turnStr$endStr"
  }.trim
}

case class Initial(comments: List[String] = Nil)

object Initial {
  val empty = Initial(Nil)
}

case class Turn(
    number: Int,
    san: String,
    comments: List[String] = Nil,
    glyphs: Glyphs = Glyphs.empty,
    opening: Option[String] = None,
    result: Option[String] = None,
    variations: List[List[Turn]] = Nil,
    // time left for the user who made the move, after he made it
    secondsLeft: Option[Int] = None
) {

  def isLong = comments.nonEmpty || variations.nonEmpty || secondsLeft.isDefined

  private def clockString: Option[String] =
    secondsLeft.map(seconds => "[%clk " + Turn.formatPsnSeconds(seconds) + "]")

  override def toString = {
    val glyphStr = glyphs.toList.map {
      case glyph if glyph.id <= 6 => glyph.symbol
      case glyph                  => s" $$${glyph.id}"
    }.mkString
    val commentsOrTime =
      if (comments.nonEmpty || secondsLeft.isDefined || opening.isDefined || result.isDefined)
        List(clockString, opening, result).flatten
          .:::(comments map Turn.noDoubleLineBreak)
          .map { text =>
            s" { $text }"
          }
          .mkString
      else ""
    val variationString =
      if (variations.isEmpty) ""
      else variations.map(_.mkString(" (", " ", ")")).mkString(" ")
    s"$number. $san$glyphStr$commentsOrTime$variationString"
  }
}

object Turn {

  private val noDoubleLineBreakRegex = "(\r?\n){2,}".r

  private def noDoubleLineBreak(txt: String) =
    noDoubleLineBreakRegex.replaceAllIn(txt, "\n")

  private def formatPsnSeconds(t: Int) =
    periodFormatter.print(
      org.joda.time.Duration.standardSeconds(t).toPeriod
    )

  private[this] val periodFormatter = new org.joda.time.format.PeriodFormatterBuilder().printZeroAlways
    .minimumPrintedDigits(1)
    .appendHours
    .appendSeparator(":")
    .minimumPrintedDigits(2)
    .appendMinutes
    .appendSeparator(":")
    .appendSeconds
    .toFormatter

}
