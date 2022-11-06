package shogi

import format.forsyth.Visual.addNewLines
import Pos._

class PlayTest extends ShogiTest {

  "playing a game" should {
    "opening one" in {
      val game =
        makeGame((SQ7G, SQ7F, false), (SQ3C, SQ3D, false), (SQ8H, SQ2B, false))
      "current game" in {
        addNewLines(game.situation.visual) must_== """
l n s g k g s n l
. r . . . . . B .
p p p p p p . p p
. . . . . . p . .
. . . . . . . . .
. . P . . . . . .
P P . P P P P P P
. . . . . . . R .
L N S G K G S N L
Hands:B
Turn:Gote
"""
      }
      "after recapture" in {
        val g = game((SQ3A, SQ2B, false))
        addNewLines(g.situation.visual) must_== """
l n s g k g . n l
. r . . . . . s .
p p p p p p . p p
. . . . . . p . .
. . . . . . . . .
. . P . . . . . .
P P . P P P P P P
. . . . . . . R .
L N S G K G S N L
Hands:Bb
Turn:Sente
"""
      }
    }
  }
}
