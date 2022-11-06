package shogi

import Pos._

class PromotionTest extends ShogiTest {

  "pawn promotion" should {
    val situation = """
. . p . . . . . .
. . . . . . . . .
K . . . . . . . .
Hands:
Turn:Gote
"""
    val game      = Game(situation)
    "promote to a tokin" in {
      game.playMove(SQ7G, SQ7H, true).situation.visual must_== stringToSituation("""
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. .+p . . . . . .
K . . . . . . . .
Hands:
Turn:Sente
""").visual
    }
    "don't force promotion by default" in {
      game.playMove(SQ7G, SQ7H).situation.visual must_== stringToSituation("""
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . p . . . . . .
K . . . . . . . .
Hands:
Turn:Sente
""").visual
    }
    "don't promote" in {
      game.playMove(SQ7G, SQ7H, false).situation.visual must_== stringToSituation("""
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . p . . . . . .
K . . . . . . . .
Hands:
Turn:Sente
""").visual
    }
    "promotion by killing" in {
      Game(
        """
. . p . . . . . .
K . R . . . . . .
Turn:Gote"""
      ).playMove(SQ7H, SQ7I, true).situation.visual must_== stringToSituation("""
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
K .+p . . . . . .
Hands:r
Turn:Sente""").visual
    }
  }
}
