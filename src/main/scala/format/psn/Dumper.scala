package shogi
package format.psn

import cats.implicits._

object Dumper {

  def apply(game: Game): List[String] = {
    val initialSfen = game.situation.history.initialSfen
    val situation   = Game(game.variant.some, initialSfen).situation
    Replay
      .situations(situation, game.moves)
      .toList
      .sliding(2)
      .map { pair =>
        apply(pair.head, pair.tail.head.history.lastMove.get)
      }
      .toList
  }

  def apply(situation: Situation, move: shogi.Move): String = move match {
    case m: PieceMove => apply(situation, m)
    case d: PieceDrop => apply(d)
  }

  def apply(situation: Situation, move: PieceMove): String = {
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
        } filter { cpos =>
          situation.moveDestsFrom(cpos).get.contains(move.dest)
        }

        val disambiguation = if (candidates.isEmpty) {
          ""
        } else if (!candidates.exists(_ ?| move.orig)) {
          move.orig.file.toString
        } else if (!candidates.exists(_ ?- move.orig)) {
          move.orig.rank.toString
        } else {
          move.orig.usiKey
        }

        s"${role.forsythUpper}$disambiguation${if (move.capture) "x" else "-"}${move.dest.usiKey}"
    }) + move.promotionString
  }

  def apply(drop: PieceDrop): String = drop.toString
}
