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
    makeReplayFromParsedMoves(makeGame(parsed.tags), op(parsed.parsedMoves))

  def fromParsedMove(
      parsedMoves: Iterable[ParsedMove],
      tags: Tags
  ): Result =
    makeReplayFromParsedMove(makeGame(tags), parsedMoves)

  private def makeReplayFromParsedMove(game: Game, parsedMoves: Iterable[ParsedMove]): Result =
    parsedMoves.foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), usi) =>
        replay
          .state(usi)
          .fold(
            err => Result.Incomplete(replay, err),
            situation => Result.Complete(Replay(game, situation))
          )
      case (r: Result.Incomplete, _) => r
    }

  private def makeReplayFromParsedMoves(game: Game, parsedMoves: ParsedMoves): Result =
    parsedMoves.value.foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), parsedMove) =>
        replay
          .state(parsedMove)
          .fold(
            err => Result.Incomplete(replay, err),
            situation => Result.Complete(Replay(game, situation))
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
    fullWithSans(pgn, identity, tags)

  def moves(moveStrs: Iterable[String], tags: Tags): Validated[String, Result] =
    movesWithSans(moveStrs, identity, tags)

  def fullWithSans(pgn: String, op: Sans => Sans, tags: Tags = Tags.empty): Validated[String, Result] =
    Parser.full(cleanUserInput(pgn)) map { parsed =>
      makeReplay(makeGame(parsed.tags ++ tags), op(parsed.sans))
    }

  def fullWithSans(parsed: ParsedPsn, op: Sans => Sans): Result =
    makeReplay(makeGame(parsed.tags), op(parsed.sans))

  def movesWithSans(moveStrs: Iterable[String], op: Sans => Sans, tags: Tags): Validated[String, Result] =
    Parser.moves(moveStrs) map { moves =>
      makeReplay(makeGame(tags), op(moves))
    }

  // remove invisible byte order mark
  def cleanUserInput(str: String) = str.replace(s"\ufeff", "")

  private def makeReplay(game: Game, sans: Sans): Result =
    sans.value.foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), san) =>
        san(replay.state.situation).fold(
          err => Result.Incomplete(replay, err),
          move => Result.Complete(replay addMove move)
        )
      case (r: Result.Incomplete, _) => r
    }
}
