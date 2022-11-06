package shogi

import format.ParsedMove

class ReplayTest extends ShogiTest {
  val parsedMoves: Seq[Seq[ParsedMove]] = Seq(Seq[ParsedMove]())

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
