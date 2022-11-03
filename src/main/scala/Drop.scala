package shogi

import shogi.format.usi.Usi

case class Drop(
    piece: Piece,
    pos: Pos,
    before: Situation,
    metrics: MoveMetrics = MoveMetrics()
) {
  def after = before(toUsi)

  def color = piece.color

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUsi = Usi.Drop(piece.role, pos)

  override def toString = toUsi.usi
}
