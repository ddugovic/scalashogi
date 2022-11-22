package shogi

import format.usi.Usi

class SituationReplayTest extends ShogiTest {
  val usis: Seq[Usis] = format.usi.Fixtures.prod500standard.map(Usi.readMoves(_).get)

  "all 500 fixtures" should {
    "have no errors and correct size" in {
      usis forall { u =>
        Replay.situations(u, None, shogi.variant.Standard) must beValid.like { case g =>
          g.tail.size must_== u.size
        }
      }
    }
  }

}
