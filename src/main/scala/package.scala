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
  type ParsedMoves   = List[format.ParsedMove]

  // TODO: remove backward compatibility code
  import shogi.format.usi.Usi
  implicit def toUsi(move: Move): Usi = move match {
    case m: PieceMove => Usi.Move(m.orig, m.dest, m.promotion)
    case d: PieceDrop => Usi.Drop(d.role, d.pos)
  }
}
