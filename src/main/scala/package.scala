import ornicar.scalalib

package object shogi extends scalalib.Common with scalalib.OrnicarOption with scalalib.OrnicarBoolean {

  val Sente = Color.Sente
  val Gote  = Color.Gote

  type Direction  = Pos => Option[Pos]
  type Directions = List[Direction]

  type PieceMap = Map[Pos, Piece]
  type HandsMap = Map[Piece, Int]
  type HandMap  = Map[Role, Int]

  type PositionHash = Array[Byte]

  type NotationMoves = Vector[format.NotationMove]

  // TODO: remove backward compatibility code
  import cats.data.Validated
  import shogi.format.{ CsaMove, KifMove, ParsedDrop, ParsedMove }
  import shogi.format.usi.Usi
  implicit def toMove(usi: Usi, situation: Situation): Move = usi match {
    case m: Usi.Move => m(situation)
    case d: Usi.Drop => d(situation)
  }
  implicit def toMove(situation: Situation, usi: Usi): Move = toMove(usi, situation)
  implicit def toUsi(move: Move): Usi = move match {
    case m: PieceMove => Usi.Move(m.orig, m.dest, m.promotion)
    case d: PieceDrop => Usi.Drop(d.role, d.pos)
  }
  implicit def toMove(parsedMove: ParsedMove, situation: Situation): Validated[String, Move] =
    parsedMove match {
      case c: CsaMove    => c.toMove(situation)
      case k: KifMove    => k.toMove(situation)
      case d: ParsedDrop => d.toMove(situation)
    }
  implicit def toMove(situation: Situation, parsedMove: ParsedMove): Validated[String, Move] =
    toMove(parsedMove, situation)
}
