package shogi

import shogi.format.usi.Usi

case class Move(
    piece: Piece,
    orig: Pos,
    dest: Pos,
    before: Situation,
    promotion: Boolean = false,
    metrics: MoveMetrics = MoveMetrics()
) {
  def after = before(toUsi)

  def capture = before.board(dest).isDefined

  def color = piece.color

  def withPromotion(p: Boolean): Move = copy(promotion = p)

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUsi = Usi.Move(orig, dest, promotion)

  override def toString = s"$piece ${toUsi.usi}"
}
