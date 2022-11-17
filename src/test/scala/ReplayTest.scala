package shogi

import format.usi.Usi

class ReplayTest extends ShogiTest {
  val usis: List[Usis] = shogi.format.usi.Fixtures.prod500standard.map(Usi.read(_).get)

  "all fixtures" should {
    "have no errors and correct size" in {
      usis forall { p =>
        val r = Replay.gamesWhileValid(p, None, shogi.variant.Standard)
        r._1.tail.size must_== p.toList.size
        r._2 must beEmpty
      }
    }
  }

}
