package shogi
package format.psn

import Pos._

class DumperTest extends ShogiTest {

  /*val outebisha = makeGame.playMoves(
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
        Dumper(outebisha) must_== "P-7f P-3d P-7e P-8d R-7h P-8e K-4h P-8f Px8f Rx8f P-7d Px7d Bx2b+ Sx2b"
            .split(' ')
            .toList
      }
    }
  }*/

  "ambiguous moves" should {
    "ambiguous file only" in {
      val situation = Game("""
k . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
P . . . K . . . P
R . . . . . . . R
""").situation
      Dumper(situation, PieceMove(Piece(Sente, Rook), SQ9I, SQ8I, false)) must_== "R9-8i"
    }
    "ambiguous rank only" in {
      val situation = Game("""
. . . . k . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . S . . .
. . . . . . . . .
. . . . K S . . .
""").situation
      Dumper(situation, PieceMove(Piece(Sente, Silver), SQ4I, SQ5H, false)) must_== "Si-5h"
    }
    "ambiguous file and rank" in {
      val situation = Game("""
. . . . k . . . .
. . . . . . . . .
. . . G . . . . .
. . . G G . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . K . . . .
""").situation
      Dumper(situation, PieceMove(Piece(Sente, Gold), SQ6D, SQ5C, false)) must_== "G6d-5c"
    }
    "unambiguous file" in {
      val situation = Game("""
. . . . k . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
P . . . . . . . P
R . . . K . . . R
""").situation
      Dumper(situation, PieceMove(Piece(Sente, Rook), SQ9I, SQ8I, false)) must_== "R-8i"
    }
    "unambiguous rank" in {
      val situation = Game("""
. . . . k . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . G . . .
. . . . . . . . .
. . . . K G . . .
""").situation
      Dumper(situation, PieceMove(Piece(Sente, Gold), SQ4I, SQ5H, false)) must_== "G-5h"
    }
  }
}
