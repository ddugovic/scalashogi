package shogi
package format
package psn

class TagTest extends ShogiTest {

  "Tags" should {
    // http://www.saremba.de/shogigml/standards/psn/psn-complete.htm#c8.1.1
    "be sorted" in {
      Tags(
        List(
          Tag(Tag.Site, "https://lishogi.org/QuCzSfxw"),
          Tag(Tag.Start, "2018.05.04"),
          Tag(Tag.End, "2018.05.04"),
          Tag(Tag.Gote, "penguingim1"),
          Tag(Tag.Sente, "DrDrunkenstein"),
          Tag(Tag.Result, "1-0"),
          Tag(Tag.UTCDate, "2018.05.04"),
          Tag(Tag.UTCTime, "20:59:23"),
          Tag(Tag.SenteElo, "2870"),
          Tag(Tag.GoteElo, "2862"),
          Tag(Tag.Event, "Titled Arena 5")
        )
      ).sorted.value.map(_.name) must_== List(
        Tag.Event,
        Tag.Site,
        Tag.Start,
        Tag.End,
        Tag.Sente,
        Tag.Gote,
        Tag.Result,
        Tag.UTCDate,
        Tag.UTCTime,
        Tag.SenteElo,
        Tag.GoteElo
      )
    }
  }
}
