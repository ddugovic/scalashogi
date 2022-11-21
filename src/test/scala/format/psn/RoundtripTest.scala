package shogi
package format
package psn

class RoundtripTest extends ShogiTest {

  "tags" should {
    "roundtrip with special chars" in {
      val value = "aä\\\"'$%/°á \t\b \"\\\\/"
      Parser.full(Psn(tags = Tags(List(Tag(_.Site, value))), moves = Vector.empty).toString) must beValid
        .like { case parsed =>
          parsed.tags("Site") must_== Some(value)
        }
    }
  }
}
