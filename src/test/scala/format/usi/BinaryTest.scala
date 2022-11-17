package shogi
package format.usi

import cats.implicits._
import scala._

import variant._

class BinaryTest extends ShogiTest {

  import BinaryTestUtils._

  val empty     = makeEmptySituation
  val situation = makeSituation

  def compareStrAndBin(usisStr: String) = {
    val usis: Usis = Usi.read(usisStr).get
    val bin        = Binary.encodeMoves(usis, Standard).toVector
    Binary.decodeMoves(bin, Standard, 600).map(_.usi).mkString(" ") must_== usisStr
  }

  def compareStrAndBin(usisStr: String, situation: Situation) = {
    val usis: Usis = Usi.read(usisStr).get
    val bin        = Binary.encodeMoves(usis, situation.variant).toVector
    Binary.decodeMove(bin, situation.variant).map(_.usi).mkString(" ") must_== usisStr
  }

  "binary encoding" should {
    "util test" in {
      showByte(parseBinary("00000101")) must_== "00000101"
      showByte(parseBinary("10100000")) must_== "10100000"
    }
    "write single move" in {
      "simple move" in {
        encodeMove("1a1b", Standard) must_== "00000000,00001001"
        encodeMove("1b1a", Standard) must_== "00001001,00000000"
        encodeMove("1a2a", Standard) must_== "00000000,00000001"
        encodeMove("7g7f", Standard) must_== "00111100,00110011"
        encodeMove("8h2b", Standard) must_== "01000110,00001010"
        encodeMove("1i1h", Standard) must_== "01001000,00111111"
        encodeMove("1i2i", Standard) must_== "01001000,01001001"
        encodeMove("9a8a", Standard) must_== "00001000,00000111"
        encodeMove("9a9b", Standard) must_== "00001000,00010001"
        encodeMove("9i8i", Standard) must_== "01010000,01001111"
        encodeMove("9h9i", Standard) must_== "01000111,01010000"
        encodeMove("1a9i", Standard) must_== "00000000,01010000"
      }
      "move with promotion symbols" in {
        encodeMove("8h2b+", Standard) must_== "01000110,10001010"
        encodeMove("8h2b=", Standard) must_== "01000110,00001010"
      }
      "drop" in {
        encodeMove("P*5e", Standard) must_== "10000001,00101000"
        encodeMove("L*6c", Standard) must_== "10000010,00010111"
        encodeMove("N*3d", Standard) must_== "10000011,00011101"
        encodeMove("S*2b", Standard) must_== "10000100,00001010"
        encodeMove("G*9a", Standard) must_== "10000101,00001000"
        encodeMove("B*9i", Standard) must_== "10000110,01010000"
        encodeMove("R*1i", Standard) must_== "10000111,01001000"
      }
      "simple move minishogi" in {
        encodeMove("1a1b", Minishogi) must_== "00000000,00000101"
        encodeMove("1a2a", Minishogi) must_== "00000000,00000001"
        encodeMove("4e5e", Minishogi) must_== "00010111,00011000"
        encodeMove("5a1e", Minishogi) must_== "00000100,00010100"
        encodeMove("B*3d", Minishogi) must_== "10000110,00010001"
      }
    }
    "write many moves" in {
      "all games" in {
        forall(format.usi.Fixtures.prod500standard) { usisStr =>
          val usis: Usis = Usi.read(usisStr).get
          val bin        = Binary.encodeMoves(usis, Standard).toList
          bin.size must be_<=(usisStr.size)
        }
      }
    }
    "read single legal move" in {
      "simple move" in {
        decodeMove("00000000,00001001", Standard) must_== "1a1b"
        decodeMove("00000000,00000001", Standard) must_== "1a2a"
        decodeMove("01010000,01000111", Standard) must_== "9i9h"
        decodeMove("00111100,00110011", Standard) must_== "7g7f"
      }
      "simple move with promotion" in {
        decodeMove("01000110,10001010", Standard) must_== "8h2b+"
        decodeMove("00000000,11010000", Standard) must_== "1a9i+"
      }
      "knight drop" in {
        //val situation = setup(empty, Piece(Sente, Knight))
        decodeMove("10000011,00011101", Standard) must_== "N*3d"
      }
      "rook drop" in {
        //val situation = setup(empty, Piece(Sente, Rook))
        decodeMove("10000111,01010000", Standard) must_== "R*9i"
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

  def encodeMove(m: String, variant: Variant): String = {
    val usis: Usis = Usi.read(m).get
    Binary.encodeMoves(usis, variant) map showByte mkString ","
  }

  def decodeMove(m: String, variant: Variant): String =
    Binary.decodeMove(m.split(',').toList.map(parseBinary), variant).head.usi

  def decodeMoves(m: String, variant: Variant): Seq[String] =
    Binary.decodeMoves(m.split(',').toList.map(parseBinary), variant, 600).map(_.usi)

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
