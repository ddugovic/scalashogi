package shogi

import format.usi.Usi

class SituationReplayTest extends ShogiTest {
  val usis: Seq[List[Usi]] = format.usi.Fixtures.prod500standard.map(Usi.readList(_).get)

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
