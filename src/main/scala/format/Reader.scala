package shogi
package format

import cats.data.Validated

import shogi.format.psn._

object Reader {

  sealed trait Result {
    def valid: Validated[String, Replay]
  }

  object Result {
    case class Complete(replay: Replay) extends Result {
      def valid = Validated.valid(replay)
    }
    case class Incomplete(replay: Replay, failures: String) extends Result {
      def valid = Validated.invalid(failures)
    }
  }

  def fromParsedNotation(parsed: ParsedNotation, op: ParsedMoves => ParsedMoves): Result =
    makeReplayFromUsis(makeGame(parsed.tags), op(parsed.parsedMoves))

  def fromParsedMoves(
      parsedMoves: ParsedMoves,
      tags: Tags
  ): Result =
    makeReplayFromUsis(makeGame(tags), parsedMoves)

  def fromUsi(
      usis: Usis,
      tags: Tags
  ): Result =
    makeReplayFromUsis(makeGame(tags), usis)

  private def makeReplayFromUsis(game: Game, usis: Usis): Result =
    usis.foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), usi) =>
        replay
          .state(usi)
          .fold(
            err => Result.Incomplete(replay, err),
            game => Result.Complete(replay(game))
          )
      case (r: Result.Incomplete, _) => r
    }

  private def makeGame(tags: Tags) =
    Game(
      variantOption = tags(_.Variant) flatMap shogi.variant.Variant.byName,
      sfen = tags.sfen
    ).copy(
      clock = tags.clockConfig map Clock.apply
    )

  def full(pgn: String, tags: Tags = Tags.empty): Validated[String, Result] =
    fullWithVariation(pgn, identity, tags)

  def moves(moveStrs: Iterable[String], tags: Tags): Validated[String, Result] =
    movesWithVariation(moveStrs, identity, tags)

  def fullWithVariation(
      pgn: String,
      op: Variation => Variation,
      tags: Tags = Tags.empty
  ): Validated[String, Result] =
    Parser.full(cleanUserInput(pgn)) map { parsed =>
      makeReplay(makeGame(parsed.tags ++ tags), op(parsed.sans))
    }

  def fullWithVariation(parsed: ParsedPsn, op: Variation => Variation): Result =
    makeReplay(makeGame(parsed.tags), op(parsed.sans))

  def movesWithVariation(
      moveStrs: Iterable[String],
      op: Variation => Variation,
      tags: Tags
  ): Validated[String, Result] =
    Parser.moves(moveStrs) map { moves =>
      makeReplay(makeGame(tags), op(moves))
    }

  // remove invisible byte order mark
  def cleanUserInput(str: String) = str.replace(s"\ufeff", "")

  private def makeReplay(game: Game, sans: Variation): Result =
    sans.value.foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), san) =>
        san(replay.state.situation).fold(
          err => Result.Incomplete(replay, err),
          move => Result.Complete(replay(move))
        )
      case (r: Result.Incomplete, _) => r
    }

  implicit def toUsis(parsedMoves: ParsedMoves): Usis = parsedMoves.toVector.map(_.usi)
}
