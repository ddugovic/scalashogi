package shogi
package format
package psn

class RenderTest extends ShogiTest {

  private def glyphs(id: Int) =
    Glyph.find(id).fold(Glyphs.empty) { g =>
      Glyphs fromList List(g)
    }

  /*
[Event "WCh"]
[Site "Bonn GER"]
[Date "2008.10.14"]
[Round "1"]
[Sente "Kramnik,V"]
[Gote "Anand,V"]
[Result "1/2-1/2"]
[SenteElo "2772"]
[GoteElo "2783"]
[Annotator "IM Malcolm Pein"]

{ It wasn't a riveting start but you don't get many risks taken in game one
when the score is still level. Kramnik asked a question, Anand answered
confidently }

1. d4 d5 2. c4 c6 3. Nc3 Nf6 4. cxd5 { The Exchange Slav, the sure way to
play with zero losing chances so an ideal choice for game one } 4... cxd5
5. Bf4 Nc6 6. e3 Bf5 7. Nf3 e6 { Gote cannot continue symmetrically for
too long of course but this is the most solid choice } 8. Qb3 Bb4 9. Bb5
O-O { Gote breaks the symmetry but this is still the main line of shogi
opening theory } 10. Bxc6 (10. O-O Bxc3 11. Bxc6 Bxb2 12. Bxb7 Bxa1 13.
   */

  "PSN string output" should {
    "be correct without variations" in {
      val psn = Psn(
        tags = Tags(
          List(
            Tag(_.Sente, "Kramnik,V"),
            Tag(_.Gote, "Anand,V")
          )
        ),
        turns = List(
          Turn(
            number = 1,
            move = Move("d4")
          ),
          Turn(
            number = 2,
            move = Move("d5")
          ),
          Turn(
            number = 3,
            move = Move("c4", glyphs = glyphs(1))
          ),
          Turn(
            number = 4,
            move = Move("c6", glyphs = glyphs(2))
          ),
          Turn(
            number = 5,
            move = Move("Nc3", glyphs = glyphs(3))
          ),
          Turn(
            number = 6,
            move = Move("Nf6")
          ),
          Turn(
            number = 7,
            move = Move(
              "cxd5",
              comments =
                "The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one" :: Nil
            )
          ),
          Turn(
            number = 8,
            move = Move("cxd5")
          ),
          Turn(
            number = 9,
            move = Move("Bf4")
          ),
          Turn(
            number = 10,
            move = Move("Nc6")
          )
        )
      )
      psn.toString must_== """[Sente "Kramnik,V"]
[Gote "Anand,V"]

1. d4 2. d5 3. c4! 4. c6? 5. Nc3!! 6. Nf6 7. cxd5 { The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one } 8. cxd5 9. Bf4 10. Nc6"""
    }
    "be correct with variations" in {
      val psn = Psn(
        tags = Tags.empty,
        turns = List(
          Turn(
            number = 1,
            move = Move(
              "d4",
              variations = List(
                List(
                  Turn(
                    number = 1,
                    move = Move("e4")
                  )
                )
              )
            )
          ),
          Turn(
            number = 2,
            move = Move(
              "Nf6",
              variations = List(
                List(
                  Turn(
                    number = 2,
                    move = Move("d5")
                  )
                )
              )
            )
          )
        )
      )
      psn.toString must_== """1. d4 (1. e4) 2. Nf6 (2. d5)"""
    }
    "result only" in {
      val psn = Psn(
        tags = Tags(
          List(
            Tag(_.Result, "0-1")
          )
        ),
        turns = List()
      )
      psn.toString must_== """[Result "0-1"]

0-1"""
    }
  }

  "initial comments" should {
    "empty" in {
      val psn = Psn(
        tags = Tags.empty,
        turns = List()
      )
      psn.toString must_== """"""
    }
    "empty with initial comment" in {
      val psn = Psn(
        tags = Tags.empty,
        turns = List(),
        initial = Initial(List("Why hello there!"))
      )
      psn.toString must_== """{ Why hello there! }"""
    }
    "empty with initial comments" in {
      val psn = Psn(
        tags = Tags.empty,
        turns = List(),
        initial = Initial(
          List(
            "Why hello there!",
            "The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one"
          )
        )
      )
      psn.toString must_== """{ Why hello there! } { The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one }"""
    }
    "moves with initial comments" in {
      val psn = Psn(
        tags = Tags.empty,
        turns = List(
          Turn(
            number = 1,
            move = Move(
              "d4",
              variations = List(
                List(
                  Turn(
                    number = 1,
                    move = Move("e4")
                  )
                )
              )
            )
          ),
          Turn(
            number = 2,
            move = Move(
              "Nf6",
              variations = List(
                List(
                  Turn(
                    number = 2,
                    move = Move("d5")
                  )
                )
              )
            )
          )
        ),
        initial = Initial(
          List(
            "Why hello there!",
            "The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one"
          )
        )
      )
      psn.toString must_== """{ Why hello there! } { The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one }
1. d4 (1. e4) 2. Nf6 (2. d5)"""
    }
  }

  "tags" should {
    "tag with \" in it" in {
      val psn = Psn(
        tags = Tags(
          List(
            Tag(_.TimeControl, "\"")
          )
        ),
        turns = List()
      )
      psn.toString must_== """[TimeControl "\""]"""
    }

    "tag with \\ in it" in {
      val psn = Psn(
        tags = Tags(
          List(
            Tag(_.TimeControl, "\\")
          )
        ),
        turns = List()
      )
      psn.toString must_== """[TimeControl "\\"]"""
    }
  }
}
