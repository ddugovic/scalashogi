package shogi
package format

import shogi.variant.Variant

object Binary {

  def decodeMoves(bs: Seq[Byte], situation: Situation, nb: Int): Vector[shogi.Move] =
    Reader.decode(bs, situation, nb)

  def encodeMoves(ms: Seq[shogi.Move], variant: Variant): Array[Byte] =
    Writer.encode(ms, variant)

  private object Encoding {
    val roleToInt: Map[Role, Int] = Map(
      King           -> 0,
      Pawn           -> 1,
      Lance          -> 2,
      Knight         -> 3,
      Silver         -> 4,
      Gold           -> 5,
      Bishop         -> 6,
      Rook           -> 7,
      Tokin          -> 8,
      PromotedLance  -> 9,
      PromotedKnight -> 10,
      PromotedSilver -> 11,
      Horse          -> 12,
      Dragon         -> 13
    )
    val intToRole: Map[Int, Role] = roleToInt map { case (k, v) => v -> k }
  }

  private object Reader {
    def decode(bs: Seq[Byte], situation: Situation, nb: Int): Vector[shogi.Move] =
      decodeMovesAndDrops(bs take (nb * 2) map toInt, situation)

    private def decodeMovesAndDrops(mds: Seq[Int], situation: Situation): Vector[Move] =
      mds
        .grouped(2)
        .map {
          case Seq(i1, i2) =>
            if (bitAt(i1, 7) && situation.variant.supportsDrops)
              decodeDrop(i1, i2, situation.variant, situation.color)
            else decodeMove(i1, i2, situation.variant, situation.board.pieces)
          case x => !!(x map showByte mkString ",")
        }
        .toVector

    private def decodeMove(i1: Int, i2: Int, variant: Variant, pieces: PieceMap): shogi.Move =
      PieceMove(
        pieces = pieces,
        orig = pos(right(i1, 7), variant.numberOfFiles),
        dest = pos(right(i2, 7), variant.numberOfFiles),
        promotion = bitAt(i2, 7)
      )

    private def decodeDrop(i1: Int, i2: Int, variant: Variant, color: Color): shogi.Move =
      PieceDrop(
        Piece(color, Encoding.intToRole(right(i1, 7))),
        pos(right(i2, 7), variant.numberOfFiles)
      )

    private def pos(i: Int, files: Int): Pos =
      Pos.at(i % files, i / files) getOrElse !!(
        s"Invalid position (files: $files, byte: ${showByte(i)})"
      )

    // right x bits
    private def right(i: Int, x: Int): Int = i & ((1 << x) - 1)
    // from right, starting at 0
    private def bitAt(i: Int, p: Int): Boolean = (i & (1 << p)) != 0

    private def !!(msg: String)          = throw new Exception("Binary move reader failed: " + msg)
    private def showByte(b: Int): String = "%08d" format (b.toBinaryString.toInt)

    @inline private def toInt(b: Byte): Int = b & 0xff
  }

  private object Writer {

    def encode(moves: Seq[shogi.Move], variant: Variant): Array[Byte] =
      moves.flatMap(encode(_, variant)).toArray

    private def encode(move: shogi.Move, variant: Variant): Seq[Byte] =
      move match {
        case PieceMove(_, orig, dest, _, prom) => encodeMove(orig, dest, prom, variant)
        case PieceDrop(piece, pos)             => encodeDrop(piece.role, pos, variant)
      }

    private def encodeMove(orig: Pos, dest: Pos, prom: Boolean, variant: Variant): Seq[Byte] =
      Seq(
        posInt(orig, variant.numberOfFiles),
        (if (prom) (1 << 7) else 0) | posInt(dest, variant.numberOfFiles)
      ).map(_.toByte)

    private def encodeDrop(role: Role, pos: Pos, variant: Variant): Seq[Byte] =
      Seq(
        (1 << 7) | Encoding.roleToInt(role),
        posInt(pos, variant.numberOfFiles)
      ).map(_.toByte)

    private def posInt(pos: Pos, files: Int): Int =
      files * pos.rank.index + pos.file.index
  }

}
