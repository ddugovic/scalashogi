package shogi
package format.psn

import Pos._

class DumperTest extends ShogiTest {

  args(skipAll = true)

  val outebisha = makeGame.playMoves(
    (SQ7G, SQ7F, false),
    (SQ3C, SQ3D, false),
    (SQ7F, SQ7E, false),
    (SQ8C, SQ8D, false),
    (SQ2H, SQ7H, false),
    (SQ8D, SQ8E, false),
    (SQ5I, SQ4H, false),
    (SQ8E, SQ8F, false),
    (SQ8G, SQ8F, false),
    (SQ8B, SQ8F, false),
    (SQ7E, SQ7D, false),
    (SQ7C, SQ7D, false),
    (SQ8H, SQ2B, true),
    (SQ3A, SQ2B, false)
  )

  "standard game" should {
    "move list" in {
      "outebisha" in {
        outebisha must beValid.like { case ms =>
          ms must_== "P-7f P-3d P-7e P-8d R-7h P-8e K-4h P-8f Px8f Rx8f P-7d Px7d Bx22+ Sx22"
            .split(' ')
            .toList
        }
      }
    }
  }

  "ambiguous moves" should {
    "ambiguous file only" in {
      val game = Game("""
k





P   K  P
R      R
""")
      game.playMoves((SQ2H, SQ8H, false)) /*map (_.psnMoves)*/ must beValid.like { case ms =>
        ms must_== List("R2-8h")
      }
    }
    "ambiguous rank only" in {
      val game = Game("""
k


 N


    K  P
 N
""")
      game.playMoves((SQ5G, SQ5H, false)) /*map (_.psnMoves)*/ must beValid.like { case ms =>
        ms must_== List("Gg-5h")
      }
    }
    "ambiguous file and rank" in {
      val game = Game("""


  G
  GG


    K
k
""")
      game.playMoves((SQ6E, SQ5D, false)) /*map (_.psnMoves)*/ must beValid.like { case ms =>
        ms must_== List("G6e-5d")
      }
    }
    "unambiguous file" in {
      val game = Game("""
k





P      P
R   K  R
""")
      game.playMoves((SQ2H, SQ4H, false)) /*map (_.psnMoves)*/ must beValid.like { case ms =>
        ms must_== List("R-4h")
      }
    }
    "unambiguous rank" in {
      val game = Game("""
k

   KGr

    G



""")
      game.playMoves((SQ5F, SQ5E, false)) /*map (_.psnMoves)*/ must beValid.like { case ms =>
        ms must_== List("G-e5")
      }
    }
  }
}
