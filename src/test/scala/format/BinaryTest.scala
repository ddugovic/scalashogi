package shogi
package format

import scala._

import shogi.Pos._
import variant._

class BinaryTest extends ShogiTest {

  import BinaryTestUtils._

  val empty     = makeEmptySituation
  val situation = makeSituation

  // isomorphic test (encode and decode trusted moves)
  def encodeThenDecodeMoves(situation: Situation, moves: Vector[shogi.Move]) = {
    val bin = Binary.encodeMoves(moves, situation.variant).toVector
    Binary.decodeMoves(bin, situation, moves.size).map(_.toString) must_== moves.map(_.toString)
  }

  "binary encoding" should {
    val king   = Piece(Sente, King)
    val rook   = Piece(Sente, Rook)
    val bishop = Piece(Sente, Bishop)
    val gold   = Piece(Sente, Gold)
    val silver = Piece(Sente, Silver)
    val knight = Piece(Sente, Knight)
    val lance  = Piece(Sente, Lance)
    val pawn   = Piece(Sente, Pawn)
    "util test" in {
      "00000101" must_== showByte(parseBinary("00000101"))
      "10100000" must_== showByte(parseBinary("10100000"))
    }
    "write single move" in {
      "sente king move" in {
        "00000000,00001001" must_== encodeMove(PieceMove(king, SQ1A, SQ1B, false))
        "00001001,00000000" must_== encodeMove(PieceMove(king, SQ1B, SQ1A, false))
        "00000000,00000001" must_== encodeMove(PieceMove(king, SQ1A, SQ2A, false))
        "00111100,00110011" must_== encodeMove(PieceMove(king, SQ7G, SQ7F, false))
        "01001000,00111111" must_== encodeMove(PieceMove(king, SQ1I, SQ1H, false))
        "01001000,01001001" must_== encodeMove(PieceMove(king, SQ1I, SQ2I, false))
        "00001000,00000111" must_== encodeMove(PieceMove(king, SQ9A, SQ8A, false))
        "00001000,00010001" must_== encodeMove(PieceMove(king, SQ9A, SQ9B, false))
        "01010000,01001111" must_== encodeMove(PieceMove(king, SQ9I, SQ8I, false))
        "01000111,01010000" must_== encodeMove(PieceMove(king, SQ9H, SQ9I, false))
      }
      "sente bishop move" in {
        "01000110,00001010" must_== encodeMove(PieceMove(bishop, SQ8H, SQ2B, false))
        "00000000,01010000" must_== encodeMove(PieceMove(bishop, SQ1A, SQ9I, false))
      }
      "move with promotion symbol" in {
        "01000110,10001010" must_== encodeMove(PieceMove(bishop, SQ8H, SQ2B, false, true))
      }
      "move with unpromotion symbol" in {
        "01000110,00001010" must_== encodeMove(PieceMove(bishop, SQ8H, SQ2B, false, false))
      }
      "drop" in {
        "10000001,00101000" must_== encodeDrop(PieceDrop(pawn, SQ5E))
        "10000010,00010111" must_== encodeDrop(PieceDrop(lance, SQ6C))
        "10000011,00011101" must_== encodeDrop(PieceDrop(knight, SQ3D))
        "10000100,00001010" must_== encodeDrop(PieceDrop(silver, SQ2B))
        "10000101,00001000" must_== encodeDrop(PieceDrop(gold, SQ9A))
        "10000110,01010000" must_== encodeDrop(PieceDrop(bishop, SQ9I))
        "10000111,01001000" must_== encodeDrop(PieceDrop(rook, SQ1I))
      }
      "simple move minishogi" in {
        "00000000,00000101" must_== encodeMove(PieceMove(king, SQ1A, SQ1B, false), Minishogi)
        "00000000,00000001" must_== encodeMove(PieceMove(king, SQ1A, SQ2A, false), Minishogi)
        "00010111,00011000" must_== encodeMove(PieceMove(king, SQ4E, SQ5E, false), Minishogi)
        "00000100,00010100" must_== encodeMove(PieceMove(bishop, SQ5A, SQ1E, false), Minishogi)
        "10000110,00010001" must_== encodeDrop(PieceDrop(bishop, SQ3D), Minishogi)
      }
    }
    "write many moves" in {
      "all games" in {
        forall(format.usi.Fixtures.prod500standard) { usisStr =>
          val usis             = shogi.format.usi.Usi.readList(usisStr).get
          val moves: Seq[Move] = Replay(situation, usis)
          shogi.format.usi.Binary.encodeMoves(usis, situation.variant) must_== Binary.encodeMoves(
            moves,
            situation.variant
          )
        }
      }
    }
    "read single move" in {
      "king move" in {
        decodeMove("00000000,00001001", empty, king, SQ1A) must_== PieceMove(king, SQ1A, SQ1B, false)
        decodeMove("00001001,00000000", empty, king, SQ1B) must_== PieceMove(king, SQ1B, SQ1A, false)
        decodeMove("00000000,00000001", empty, king, SQ1A) must_== PieceMove(king, SQ1A, SQ2A, false)
        decodeMove("01000111,01010000", empty, king, SQ9H) must_== PieceMove(king, SQ9H, SQ9I, false)
        decodeMove("00111100,00110011", empty, king, SQ7G) must_== PieceMove(king, SQ7G, SQ7F, false)
      }
      "bishop move with promotion" in {
        decodeMove("01000110,10001010", empty, bishop, SQ8H) must_== PieceMove(
          bishop,
          SQ8H,
          SQ2B,
          false,
          true
        )
        decodeMove("00000000,11010000", empty, bishop, SQ1A) must_== PieceMove(
          bishop,
          SQ1A,
          SQ9I,
          false,
          true
        )
      }
      "knight drop and rook drop" in {
        decodeDrop("10000011,00011101", empty) must_== PieceDrop(knight, SQ3D)
        decodeDrop("10000111,01010000", empty) must_== PieceDrop(rook, SQ9I)
      }
    }
    "be isomorphic" in {
      // test isomorphism (including illegal drops and moves)
      val variant = empty.variant
      "for standard square combinations" in {
        forall(Piece.all) { piece =>
          forall(variant.allPositions) { pos =>
            val situation = setup(empty, piece, pos)
            val pieceMoves = for {
              dest      <- variant.allPositions
              promotion <- Seq(false, true)
            } yield PieceMove(piece, pos, dest, dest == pos, promotion)
            encodeThenDecodeMoves(situation, pieceMoves.toVector)
          }
        }
      }
      "for standard drop combinations" in {
        val allDrops = for {
          piece <- Piece.all
          pos   <- variant.allPositions
        } yield PieceDrop(piece, pos)
        encodeThenDecodeMoves(empty, allDrops.toVector)
      }
    }
  }

}

object BinaryTestUtils {

  def setup(empty: Situation, piece: Piece, pos: Pos): Situation = empty
    .withHands(empty.hands.store(piece, 1)) apply PieceDrop(piece, pos)

  def showByte(b: Byte): String =
    "%08d" format {
      b & 0xff
    }.toBinaryString.toInt

  def encodeMove(m: PieceMove, variant: Variant = Standard): String =
    Binary.encodeMoves(Seq(m), variant) map showByte mkString ","

  def encodeDrop(d: PieceDrop, variant: Variant = Standard): String =
    Binary.encodeMoves(Seq(d), variant) map showByte mkString ","

  def decodeMove(m: String, empty: Situation, piece: Piece, pos: Pos): Move = {
    val situation = setup(empty, piece, pos)
    Binary.decodeMoves(m.split(',').toList.map(parseBinary), situation, 1).head
  }

  def decodeDrop(m: String, empty: Situation): Move =
    Binary.decodeMoves(m.split(',').toList.map(parseBinary), empty, 1).head

  def parseBinary(s: String): Byte = {
    var i    = s.size - 1
    var sum  = 0
    var mult = 1
    while (i >= 0) {
      s.charAt(i) match {
        case '1' => sum += mult
        case '0' =>
        case x   => sys error s"invalid binary literal: $x in $s"
      }
      mult *= 2
      i -= 1
    }
    sum.toByte
  }
}
