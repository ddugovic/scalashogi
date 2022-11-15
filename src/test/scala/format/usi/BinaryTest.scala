package shogi
package format.usi

import scala._

import format.forsyth.Sfen
import variant._

class BinaryTest extends ShogiTest {

  import BinaryTestUtils._

  val empty     = makeEmptySituation
  val situation = makeSituation

  def compareStrAndBin(usisStr: String) = {
    val usis: List[Usi]  = Usi.readList(usisStr).get
    val moves: Usi.Moves = Usi.Moves(usis, situation)
    val bin              = Binary.encodeMoves(moves, situation.variant).toVector
    Binary.decodeMoves(bin, situation, 600).map(_.usi).mkString(" ") must_== usisStr
  }

  def compareStrAndBin(usisStr: String, situation: Situation) = {
    val usis: List[Usi]  = Usi.readList(usisStr).get
    val moves: Usi.Moves = Usi.Moves(usis, situation)
    val bin              = Binary.encodeMoves(moves, situation.variant).toVector
    Binary.decodeMove(bin, situation).map(_.usi).mkString(" ") must_== usisStr
  }

  "binary encoding" should {
    "util test" in {
      showByte(parseBinary("00000101")) must_== "00000101"
      showByte(parseBinary("10100000")) must_== "10100000"
    }
    "write single move" in {
      "simple move" in {
        val sfen = Sfen(
          "KKKKKKKKK/KKKKKKKKK/KKKKKKKKK/KKKKKKKKK/KKKKKKKKK/KKKKKKKKK/KKKKKKKKK/KKKKKKKKK/KKKKKKKKK b - 1"
        )
        val situation = sfen.toSituation(Standard).get
        encodeMove("1a1b", situation) must_== "00000000,00001001"
        encodeMove("1b1a", situation) must_== "00001001,00000000"
        encodeMove("1a2a", situation) must_== "00000000,00000001"
        encodeMove("7g7f", situation) must_== "00111100,00110011"
        encodeMove("8h2b", situation) must_== "01000110,00001010"
        encodeMove("1i1h", situation) must_== "01001000,00111111"
        encodeMove("1i2i", situation) must_== "01001000,01001001"
        encodeMove("9a8a", situation) must_== "00001000,00000111"
        encodeMove("9a9b", situation) must_== "00001000,00010001"
        encodeMove("9i8i", situation) must_== "01010000,01001111"
        encodeMove("9h9i", situation) must_== "01000111,01010000"
        encodeMove("1a9i", situation) must_== "00000000,01010000"
      }
      "move with promotion symbols" in {
        encodeMove("8h2b+", situation) must_== "01000110,10001010"
        encodeMove("8h2b=", situation) must_== "01000110,00001010"
      }
      "drop" in {
        val sfen      = Sfen("4k4/9/9/9/9/9/9/9/4K4 b RB2G2S2N2L9Prb2g2s2n2l9p 1")
        val situation = sfen.toSituation(Standard).get
        encodeMove("P*5e", situation) must_== "10000001,00101000"
        encodeMove("L*6c", situation) must_== "10000010,00010111"
        encodeMove("N*3d", situation) must_== "10000011,00011101"
        encodeMove("S*2b", situation) must_== "10000100,00001010"
        encodeMove("G*9a", situation) must_== "10000101,00001000"
        encodeMove("B*9i", situation) must_== "10000110,01010000"
        encodeMove("R*1i", situation) must_== "10000111,01001000"
      }
      "simple move minishogi" in {
        val minishogi = Situation(Minishogi)
        encodeMove("1a1b", minishogi) must_== "00000000,00000101"
        encodeMove("1a2a", minishogi) must_== "00000000,00000001"
        encodeMove("4e5e", minishogi) must_== "00010111,00011000"
        encodeMove("5a1e", minishogi) must_== "00000100,00010100"
        val minishogiWithBishop = setup(minishogi, Piece(Sente, Bishop))
        encodeMove("B*3d", minishogiWithBishop) must_== "10000110,00010001"
      }
    }
    "write many moves" in {
      "all games" in {
        forall(format.usi.Fixtures.prod500standard) { usisStr =>
          val usis: List[Usi]  = Usi.readList(usisStr).get
          val moves: Usi.Moves = Usi.Moves(usis, situation)
          val bin              = Binary.encodeMoves(moves, situation.variant).toList
          bin.size must be_<=(usisStr.size)
        }
      }
    }
    "read single legal move" in {
      "simple move" in {
        decodeMove("00000000,00001001", situation) must_== "1a1b"
        decodeMove("00000000,00000001", situation) must_== "1a2a"
        decodeMove("01010000,01000111", situation) must_== "9i9h"
        decodeMove("00111100,00110011", situation) must_== "7g7f"
      }
      "simple move with promotion" in {
        decodeMove("01000110,10001010", situation) must_== "8h2b+"
        decodeMove("00000000,11010000", situation) must_== "1a9i+"
      }
      "knight drop" in {
        val situation = setup(empty, Piece(Sente, Knight))
        decodeMove("10000011,00011101", situation) must_== "N*3d"
      }
      "rook drop" in {
        val situation = setup(empty, Piece(Sente, Rook))
        decodeMove("10000111,01010000", situation) must_== "R*9i"
      }
    }
    "be isomorphic" in {
      "for one game" in {
        compareStrAndBin(format.usi.Fixtures.prod500standard.head)
      }
      "for all games" in {
        forall(format.usi.Fixtures.prod500standard)(compareStrAndBin)
      }
    }
    "for standard moves" in {
      val allMoves = for {
        orig <- Pos.all
        dest <- Pos.all
      } yield Usi.Move(orig, dest, false)
      forall(allMoves) { move =>
        val situation = setup(empty, Piece(Sente, King), move.orig)
        compareStrAndBin(move.usiKeys, situation)
        compareStrAndBin(move.usi, situation)
      }
    }
    "for standard drops" in {
      val allDrops = for {
        role <- Standard.handRoles
        pos  <- Pos.all
      } yield Usi.Drop(role, pos)
      forall(allDrops) { drop =>
        val situation = setup(empty, Piece(Sente, drop.role))
        compareStrAndBin(drop.usi, situation)
      }
    }
  }

}

object BinaryTestUtils {

  def setup(empty: Situation, piece: Piece): Situation =
    empty.withHands(empty.hands.store(piece, 1))

  def setup(empty: Situation, piece: Piece, pos: Pos): Situation =
    setup(empty, piece) apply PieceDrop(piece, pos)

  def showByte(b: Byte): String =
    "%08d" format {
      b & 0xff
    }.toBinaryString.toInt

  def encodeMove(m: String, situation: Situation): String = {
    val usis: List[Usi]  = Usi.readList(m).get
    val moves: Usi.Moves = Usi.Moves(usis, situation)
    Binary.encodeMoves(moves, situation.variant) map showByte mkString ","
  }

  def decodeMove(m: String, situation: Situation): String =
    Binary.decodeMove(m.split(',').toList.map(parseBinary), situation).head.usi

  def decodeMoves(m: String, situation: Situation): Seq[String] =
    Binary.decodeMoves(m.split(',').toList.map(parseBinary), situation, 600).map(_.usi)

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
