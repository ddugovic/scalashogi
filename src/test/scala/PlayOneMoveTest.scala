package shogi

import Pos._

class PlayOneMoveTest extends ShogiTest {

  "playing a move" should {
    "only process things once" in {
      makeGame((SQ7G, SQ7F, false)).moves.size must_== 1
    }
  }
}
