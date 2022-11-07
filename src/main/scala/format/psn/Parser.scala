package shogi
package format
package psn

import cats.parse.{ LocationMap, Numbers => N, Parser => P, Parser0 => P0, Rfc5234 => R }
import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._
import cats.parse.Parser.Expectation
import cats.data.NonEmptyList

// http://www.saremba.de/shogigml/standards/pgn/pgn-complete.htm
object Parser {

  val whitespace = R.cr | R.lf | R.wsp
  val psnComment = P.caret.filter(_.col == 0) *> P.char('%') *> P.until(P.char('\n')).void
  // psnComment with % or whitespaces
  val escape = psnComment.? *> whitespace.rep0.?

  def full(psn: String): Validated[String, ParsedPsn] =
    psnParser.parse(psn) match {
      case Right((_, parsedResult)) =>
        valid(parsedResult)
      case Left(err) =>
        invalid(showExpectations("Cannot parse psn", psn, err))
    }

  def moves(str: String): Validated[String, Sans] =
    MovesParser.moves(str)

  def moves(strMoves: Iterable[String]): Validated[String, Sans] =
    strMoves.toList
      .traverse(MovesParser.move)
      .map(Sans(_))

  def move(str: String): Validated[String, San] = MovesParser.move(str)

  private object MovesParser {

    private def cleanComments(comments: List[String]) = comments.map(_.trim).filter(_.nonEmpty)

    def moves(str: String): Validated[String, Sans] =
      strMove.rep.map(xs => Sans(xs.toList)).parse(str) match {
        case Right((_, str)) =>
          valid(str)
        case Left(err) => invalid(showExpectations("Cannot parse moves", str, err))
      }

    def move(str: String): Validated[String, San] =
      strMove.parse(str) match {
        case Right((_, str)) =>
          valid(str)
        case Left(err) => invalid(showExpectations("Cannot parse move", str, err))
      }

    val blockCommentary: P[String] = P.until0(P.char('}')).with1.between(P.char('{'), P.char('}'))

    val inlineCommentary: P[String] = P.char(';') *> P.until(R.lf)

    val commentary = (blockCommentary | inlineCommentary).withContext("Invalid comment") <* escape
    val resultList = List(
      "*",
      "1/2-1/2",
      "½-½",
      "0-1",
      "1-0",
      "1/2‑1/2",
      "½‑½",
      "0‑1",
      "1‑0",
      "1/2–1/2",
      "½–½",
      "0–1",
      "1–0"
    )

    def mapResult(result: String): String = result match {
      case "½-½"     => "1/2-1/2"
      case "1/2‑1/2" => "1/2-1/2"
      case "½‑½"     => "1/2-1/2"
      case "1/2–1/2" => "1/2-1/2"
      case "½–½"     => "1/2-1/2"
      case "0‑1"     => "0-1"
      case "0–1"     => "0-1"
      case "1‑0"     => "1-0"
      case "1–0"     => "1-0"
      case x         => x
    }

    val result: P[String] = P.stringIn(resultList).map(mapResult)

    val nagGlyphsRE = P.stringIn(
      Glyph.PositionAssessment.all
        .map(_.symbol)
        .sortBy(-_.length)
    )

    val nag = (P.char('$') ~ R.digit.rep).string | nagGlyphsRE

    val nagGlyphs: P0[Glyphs] = (nag <* escape).rep0.map(nags =>
      Glyphs fromList nags.flatMap {
        Glyph.find
      }
    )

    val moveExtras = commentary.void

    val positiveIntString: P[String] =
      (N.nonZeroDigit ~ N.digits0).string

    // '. ' or '... ' or '. ... '
    val numberSuffix = (P.char('.') | whitespace).rep0.void

    // 10. or 10... but not 0 or 1-0 or 1/2
    val number = (positiveIntString <* !P.charIn('‑', '–', '-', '/') ~ numberSuffix).string

    val forbidNullMove =
      P.stringIn(List("--", "Z0", "null", "pass", "@@@@"))
        .?
        .flatMap(o => o.fold(P.unit)(_ => P.failWith("Lishogi does not support null moves").void))

    val strMove: P[San] = P
      .recursive[San] { recuse =>
        val variation: P[Sans] =
          (((P.char('(') <* escape) *> recuse.rep0 <* (P.char(')') ~ escape)) <* escape)
            .map(Sans(_))

        ((number.backtrack | (commentary <* escape)).rep0 ~ forbidNullMove).with1 *>
          (((MoveParser.moveWithGlyphs ~ nagGlyphs ~ commentary.rep0 ~ nagGlyphs ~ variation.rep0) <* moveExtras.rep0) <* escape).backtrack
            .map { case ((((san, glyphs), comments), glyphs2), variations) =>
              san withComments comments withVariations variations mergeGlyphs (glyphs merge glyphs2)
            }
      }

    val strMoves: P0[(InitialPosition, List[San], Option[String])] =
      ((commentary.rep0 ~ strMove.rep0) ~ (result <* escape).? <* commentary.rep0).map {
        case ((coms, sans), res) => (InitialPosition(cleanComments(coms)), sans.toList, res)
      }
  }

  private object MoveParser {

    def rangeToMap(r: Iterable[Char]) = r.zipWithIndex.to(Map).view.mapValues(_ + 1)

    val fileMap = rangeToMap('1' to '9')
    val rankMap = rangeToMap('a' to 'i')

    val glyph: P[Glyph] =
      mapParser(
        Glyph.MoveAssessment.all.sortBy(_.symbol.length).map { g =>
          g.symbol -> g
        },
        "glyph"
      )

    val glyphs = glyph.rep0.map(gs => Glyphs.fromList(gs))

    val x = P.charIn("-x").?.map(_.getOrElse('-').equals('x'))

    val role = mapParser(Role.allByForsythUpper, "role")

    val file = mapParserChar(fileMap, "file")

    val rank = mapParserChar(rankMap, "rank")

    val dest: P[Pos] = mapParser(Pos.allUsiKeys, "dest")

    val plus = P.charIn("+=").?.map(_.getOrElse('=').equals('+'))

    // G-5b
    val ambiguous: P[Std] = (role ~ x ~ dest ~ plus).map { case (((ro, ca), de), pr) =>
      Std(dest = de, role = ro, capture = ca, promotion = pr)
    }

    // G@5b
    val drop: P[Drp] = ((role <* P.charIn("@'*")) ~ dest).map { case (role, pos) => Drp(role, pos) }

    // G6-5b G6x5b Ga-5b Gax5b G6a-5b G6ax5b
    val disambiguated: P[Std] = (role ~ file.? ~ rank.? ~ x ~ dest ~ plus).map {
      case (((((ro, fi), ra), ca), de), pr) =>
        Std(dest = de, role = ro, capture = ca, file = fi, rank = ra, promotion = pr)
    }

    val standard: P[San] = P.oneOf(
      (disambiguated :: ambiguous :: drop :: Nil).map(_.backtrack)
    )

    val move: P[San] = standard.withContext("Invalid shogi move")
    val moveWithGlyphs: P[San] = (move ~ glyphs <* escape)
      .map { case (std, gly) =>
        std mergeGlyphs gly
      }

    def mapParser[A](pairs: Iterable[(String, A)], name: String): P[A] = {
      val pairMap = pairs.to(Map)
      P.stringIn(pairMap.keySet).map(pairMap(_)) | P.failWith(name + " not found")
    }

    def mapParserChar[A](pairs: Iterable[(Char, A)], name: String): P[A] = {
      val pairMap = pairs.to(Map)
      P.charIn(pairMap.keySet).map(pairMap(_)) | P.failWith(name + " not found")
    }

  }

  private object TagParser {

    val tagName: P[String]         = R.alpha.rep.string.withContext("Tag name can only contains alphabet characters")
    val escaped: P[String]         = P.char('\\') *> (R.dquote | P.char('\\')).string
    val valueChar: P[String]       = escaped | P.charWhere(_ != '"').string
    val tagValue: P[String]        = valueChar.rep0.map(_.mkString).with1.surroundedBy(R.dquote)
    val tagContent: P[Tag]         = ((tagName <* R.wsp.rep) ~ tagValue).map(p => Tag(p._1, p._2))
    val tag: P[Tag]                = tagContent.between(P.char('['), P.char(']')) <* whitespace.rep0
    val tags: P[NonEmptyList[Tag]] = tag.rep

  }

  private val tagsAndMovesParser: P0[ParsedPsn] =
    ((escape *> TagParser.tags.? <* escape) ~ MovesParser.strMoves.?).map {
      case (optionalTags, optionalMoves) => {
        val preTags = Tags(optionalTags.map(_.toList).getOrElse(List()))
        optionalMoves match {
          case None => ParsedPsn(InitialPosition(List()), preTags, Sans(List()))
          case Some((init, sans, resultOption)) => {
            val tags =
              resultOption.filterNot(_ => preTags.exists(_.Result)).foldLeft(preTags)(_ + Tag(_.Result, _))
            ParsedPsn(init, tags, Sans(sans))
          }
        }
      }
    }

  private val psnParser: P0[ParsedPsn] =
    escape *> P.string("[psn]").? *> tagsAndMovesParser <* P.string("[/psn]").? <* escape

  private def showExpectations(context: String, str: String, error: P.Error): String = {
    val lm  = LocationMap(str)
    val idx = error.failedAtOffset
    val caret = lm.toCaret(idx).getOrElse {
      throw new RuntimeException("This is impossible")
    }
    val line         = lm.getLine(caret.line).getOrElse("")
    val errorLine    = line ++ "\n" ++ " ".repeat(caret.col) ++ "^"
    val errorMessage = s"$context: [${caret.line + 1}.${caret.col + 1}]: ${expToString(error.expected.head)}"
    errorMessage ++ "\n\n" ++ errorLine ++ "\n" ++ str
  }

  private def expToString(expectation: Expectation): String =
    expectation match {
      case Expectation.OneOfStr(_, strs) =>
        strs match {
          case one :: Nil => s"expected: $one"
          case _          => s"expected one of: $strs"
        }
      case Expectation.InRange(_, lower, upper) =>
        if (lower == upper) {
          s"expected: $lower"
        } else {
          s"expected char in range: [$lower, $upper]"
        }
      case Expectation.StartOfString(_)       => "expected start of the file"
      case Expectation.EndOfString(_, length) => s"expected end of file but $length characters remaining"
      case Expectation.Length(_, expected, actual) =>
        s"expected $expected more characters but only $actual remaining"
      case Expectation.ExpectedFailureAt(_, matched) =>
        s"expected failure but the parser matched: $matched"
      case Expectation.Fail(_)                    => "Failed"
      case Expectation.FailWith(_, message)       => message
      case Expectation.WithContext(contextStr, _) => contextStr
    }
}