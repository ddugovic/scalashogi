package shogi

import format.ParsedMove
import format.usi.Usi

class ReplayTest extends ShogiTest {
  val usis: List[List[Usi]]               = shogi.format.usi.Fixtures.prod500standard.map(Usi.readList(_).get)
  val parsedMoves: List[List[ParsedMove]] = usis map parseTrustedUsis

  "all fixtures" should {
    "have no errors and correct size" in {
      parsedMoves forall { p =>
        val r = Replay.gamesWhileValid(p, None, shogi.variant.Standard)
        r._1.tail.size must_== p.size
        r._2 must beEmpty
      }
    }
  }

}
