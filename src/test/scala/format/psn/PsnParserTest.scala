package shogi
package format
package psn

import cats.syntax.option._

class PsnParserTest extends ShogiTest {

  import Fixtures._

  val parser    = Parser.full _
  val parseMove = Parser.move _

  "psnComment" should {
    "parse valid comment" in {
      Parser.psnComment.parse("% comment") must beRight
    }
    "parse invalid comment" in {
      Parser.psnComment.parse("  %comment") must beLeft
    }
  }

  "pawn move" should {
    "P-2f" in {
      parseMove("P-2f") must beValid.like { case san: Std =>
        san.promotion must beFalse
      }
    }
  }

  "promotion check" should {
    "promote (no capture)" in {
      parser("N-5c+ ") must beValid.like { case a =>
        a.sans.value.headOption must beSome.like { case san: Std =>
          san.promotion must beTrue
        }
      }
    }
    "promote (capture)" in {
      parser("Nx5c+ ") must beValid.like { case a =>
        a.sans.value.headOption must beSome.like { case san: Std =>
          san.promotion must beTrue
        }
      }
    }
    "unpromote (no capture)" in {
      parser("N-5c= ") must beValid.like { case a =>
        a.sans.value.headOption must beSome.like { case san: Std =>
          san.promotion must beFalse
        }
      }
    }
    "unpromote (capture)" in {
      parser("Nx5c= ") must beValid.like { case a =>
        a.sans.value.headOption must beSome.like { case san: Std =>
          san.promotion must beFalse
        }
      }
    }
  }

  "carriage return" in {
    "none" in {
      parser("1. P-2f\n2. P-8d") must beValid.like { case parsed =>
        parsed.sans.value.size must_== 2
      }
    }
    "one" in {
      parser("1. P-2f\r\n2. P-8d") must beValid.like { case parsed =>
        parsed.sans.value.size must_== 2
      }
    }
    "two" in {
      parser("1. P-2f\r\r\n2. P-8d") must beValid.like { case parsed =>
        parsed.sans.value.size must_== 2
      }
    }
    "between tags" in {
      parser("[Sente \"carriage\"]\r\n[Gote \"return\"]\r\n\r\n1. P-2f 2. P-8d\r\n") must beValid.like {
        case parsed =>
          parsed.tags(_.Sente) must_== Some("carriage")
          parsed.tags(_.Gote) must_== Some("return")
          parsed.sans.value.size must_== 2
      }
    }
  }

  "result" in {
    "no tag but inline result" in {
      parser(noTagButResult) must beValid.like { case parsed =>
        parsed.tags("Result") must_== Option("1-0")
      }
    }
  }

  "glyphs" in {
    parseMove("P-2f") must beValid.like { case a: Std =>
      a must_== Std(Pos.SQ2F, Pawn)
    }
    parseMove("P-2f!") must beValid.like { case a: Std =>
      a.dest === Pos.SQ2F
      a.role === Pawn
      a.metas.glyphs === Glyphs(Glyph.MoveAssessment.good.some, None, Nil)
    }
    parseMove("G6a-5b?!") must beValid.like { case a: Std =>
      a.file === Some(6)
      a.rank === Some(1)
      a.dest === Pos.SQ5B
      a.role === Gold
      a.metas.glyphs === Glyphs(Glyph.MoveAssessment.dubious.some, None, Nil)
    }
    parseMove("G4a-5b!") must beValid.like { case a: Std =>
      a.file === Some(4)
      a.rank === Some(1)
      a.dest === Pos.SQ5B
      a.role === Gold
      a.metas.glyphs === Glyphs(Glyph.MoveAssessment.good.some, None, Nil)
    }
    parseMove("P@2g?!") must beValid.like { case a: Drp =>
      a.dest === Pos.SQ2G
      a.role === Pawn
      a.metas.glyphs === Glyphs(Glyph.MoveAssessment.dubious.some, None, Nil)
    }
  }

  "nags" in {
    parser(withNag) must beValid

    parser("Rx3d+! $13") must beValid.like { case ParsedPsn(_, _, Variation(List(san))) =>
      san.metas.glyphs.move must_== Option(Glyph.MoveAssessment.good)
      san.metas.glyphs.position must_== Option(Glyph.PositionAssessment.unclear)
    }
  }

  "non-nags" in {
    parser("Bx2b?? ∞") must beValid.like { case ParsedPsn(_, _, Variation(List(san))) =>
      san.metas.glyphs.move must_== Option(Glyph.MoveAssessment.blunder)
      san.metas.glyphs.position must_== Option(Glyph.PositionAssessment.unclear)
    }
  }

  "comments" in {
    parser("P-2d! {such a neat comment}") must beValid.like { case ParsedPsn(_, _, Variation(List(san))) =>
      san.metas.comments must_== List("such a neat comment")
    }
  }

  "variations" in {
    parser("P-2d! {such a neat comment} (Px2d Bx2d)") must beValid.like {
      case ParsedPsn(_, _, Variation(List(san))) =>
        san.metas.variations.headOption must beSome.like { case variation =>
          variation.value must haveSize(2)
        }
    }
  }

  "first move variation" in {
    parser("1. P-2f (1. P-7f)") must beValid.like { case ParsedPsn(_, _, Variation(List(san))) =>
      san.metas.variations.headOption must beSome.like { case variation =>
        variation.value must haveSize(1)
      }
    }
  }

  raws foreach { sans =>
    val size = sans.split(' ').length
    "sans only size: " + size in {
      parser(sans) must beValid.like { case a =>
        a.sans.value.size must_== size
      }
    }
  }

  "variations" in {
    parser(variations) must beValid.like { case a =>
      a.sans.value.size must_== 76
    }
  }

  "inline tags" in {
    parser(inlineTags) must beValid.like { case a =>
      a.tags.value must contain { (tag: Tag) =>
        tag.name == Tag.Sente && tag.value == "Miyamoto Toyokazu"
      }
    }
  }

  "tag with nested quotes" in {
    parser("""[Gote "Schwarzenegger, Arnold \"The Terminator\""]""") must beValid.like { case a =>
      a.tags.value must contain { (tag: Tag) =>
        tag.name == Tag.Gote && tag.value == """Schwarzenegger, Arnold "The Terminator""""
      }
    }
  }

  "tag with inner brackets" in {
    parser("""[Gote "[=0040.34h5a4]"]""") must beValid.like { case a =>
      a.tags.value must contain { (tag: Tag) =>
        tag.name == Tag.Gote && tag.value == "[=0040.34h5a4]"
      }
    }
  }

  "inline comments" in {
    parser(inlineComments) must beValid.like { case a =>
      a.sans.value.size must_== 76
    }
  }

  "comments and variations" in {
    parser(commentsAndVariations) must beValid.like { case a =>
      a.sans.value.size must_== 76
    }
  }

  "year" in {
    "full date" in {
      parser(europeHoskingPotter) must beValid.like { case parsed =>
        parsed.tags.year must_== Option(1997)
      }
    }
  }

}
