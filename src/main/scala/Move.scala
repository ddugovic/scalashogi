package shogi

sealed trait Move

case class PieceMove(
    piece: Piece,
    orig: Pos,
    dest: Pos,
    capture: Boolean,
    promotion: Boolean = false
) extends Move {
  def withCapture(c: Boolean): PieceMove = copy(capture = c)

  def withPromotion(p: Boolean): PieceMove = copy(promotion = p)

  def usi = shogi.format.usi.Usi.Move(dest, orig)

  def captureString = if (capture) "x" else "-"

  def promotionString = if (promotion) "+" else ""

  override def toString = s"${piece.role}$orig$captureString$dest$promotionString"
}

object PieceMove {
  def apply(pieces: PieceMap, orig: Pos, dest: Pos, promotion: Boolean): PieceMove =
    PieceMove(pieces(orig), orig, dest, pieces.contains(dest), promotion)

  def apply(board: Board, piece: Piece, orig: Pos, dest: Pos, promotion: Boolean): PieceMove =
    PieceMove(piece, orig, dest, capture = board(dest).isDefined, promotion)

  def apply(board: Board, orig: Pos, dest: Pos, promotion: Boolean): PieceMove =
    apply(board, board(orig).get, orig, dest, promotion)

  def apply(situation: Situation, piece: Piece, orig: Pos, dest: Pos, promotion: Boolean): PieceMove =
    apply(situation.board, piece, orig, dest, promotion)

  def apply(situation: Situation, orig: Pos, dest: Pos, promotion: Boolean): PieceMove =
    apply(situation.board, orig, dest, promotion)
}

case class PieceDrop(
    piece: Piece,
    pos: Pos
) extends Move {
  def role = piece.role

  def usi = shogi.format.usi.Usi.Drop(role, pos)

  override def toString = s"${piece.role}*${pos}"
}

object PieceDrop {
  def apply(color: Color, role: Role, pos: Pos): PieceDrop =
    PieceDrop(Piece(color, role), pos)

  def apply(situation: Situation, role: Role, pos: Pos): PieceDrop =
    apply(situation.color, role, pos)
}
