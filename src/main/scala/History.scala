package shogi

import shogi.format.forsyth.Sfen

case class History(
    lastMove: Option[Move],
    positionHashes: PositionHash,
    initialSfen: Option[Sfen]
) {

  private def isRepetition(times: Int) =
    positionHashes.length > (times - 1) * 4 * Hash.size && {
      // compare only hashes for positions with the same side to move
      val positions = positionHashes.sliding(Hash.size, 2 * Hash.size).toList
      positions.headOption match {
        case Some(Array(x, y, z)) =>
          (positions count {
            case Array(x2, y2, z2) => x == x2 && y == y2 && z == z2
            case _                 => false
          }) >= times
        case _ => times <= 1
      }
    }

  lazy val fourfoldRepetition = isRepetition(4)

  def withLastMove(m: Move) = copy(lastMove = Some(m))

  def withPositionHashes(h: PositionHash) = copy(positionHashes = h)

  def withInitialSfen(s: Sfen) = copy(initialSfen = Some(s))

  override def toString = {
    val positions = (positionHashes grouped Hash.size).toList
    s"${lastMove} ${positions.map(Hash.debug).mkString(" ")} ${initialSfen.getOrElse("-")}"
  }
}

object History {

  def empty: History = History(None, Array.empty, None)

}
