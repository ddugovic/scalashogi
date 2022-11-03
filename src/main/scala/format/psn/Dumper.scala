package shogi
package format.psn

object Dumper {

  def apply(situation: Situation, move: shogi.Move): String = {
    (move.piece.role match {
      case (role) =>
        // Check whether there is a need to disambiguate:
        //   - can a piece of same role move to/capture on the same square?
        //   - if so, disambiguate, in order or preference, by:
        //       - file
        //       - rank
        //       - both (only happens w/ at least 3 pieces of the same role)
        val candidates = situation.board.pieces collect {
          case (cpos, cpiece) if cpiece == move.piece && cpos != move.orig && cpiece.eyes(cpos, move.dest) =>
            cpos
        } /*filter { cpos =>
          situation.move(cpos, move.dest).isValid
        }*/

        val disambiguation = if (candidates.isEmpty) {
          ""
        } else if (!candidates.exists(_ ?| move.orig)) {
          move.orig.file.toString
        } else if (!candidates.exists(_ ?- move.orig)) {
          move.orig.rank.toString
        } else {
          move.orig.usiKey
        }

        s"${role.forsythUpper}$disambiguation${if (move.capture) "x" else ""}${move.dest.usiKey}"
    }) + move.toUsi.promotionString
  }

  def apply(data: shogi.Move): String =
    apply(
      data.before,
      data
    )

  def apply(drop: shogi.Drop): String = drop.toUsi.usi
}
