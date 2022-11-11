package shogi

import cats.data.Validated
import cats.data.Validated.{ invalid, valid, Invalid, Valid }
import cats.data.NonEmptyList
import cats.implicits._

import shogi.format.{ ParsedMove, Reader, Tag, Tags }
import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi;

case class Replay(setup: Game, state: Game) {
  def apply(game: Game) = copy(state = game)

  def apply(move: Move) = copy(state = state(move))
}

object Replay {

  def apply(game: Game) = new Replay(game, game)

  def apply(
      parsedMoves: List[ParsedMove],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): Reader.Result =
    Reader.fromParsedMove(
      parsedMoves,
      Tags(
        List(
          initialSfen map { sfen =>
            Tag(_.Sfen, sfen.value)
          },
          variant.some.filterNot(_.standard) map { v =>
            Tag(_.Variant, v.name)
          }
        ).flatten
      )
    )

  def apply(
      parsedMoves: Seq[ParsedMove],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): Reader.Result = apply(parsedMoves.toList, initialSfen, variant)

  def apply(
      parsedMoves: Vector[ParsedMove],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): Reader.Result = apply(parsedMoves.toList, initialSfen, variant)

  def replay(
      parsedMoves: List[ParsedMove],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): Validated[String, Replay] =
    parsedMoves.foldLeft[Validated[String, Replay]](valid(Replay(makeGame(initialSfen, variant)))) {
      case (acc, parsedMove) =>
        acc andThen { replay =>
          replay.state(parsedMove) andThen { game =>
            valid(replay(game))
          }
        }
    }

  def gamesWhileValid(
      parsedMoves: List[ParsedMove],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): (NonEmptyList[Game], Option[String]) = {

    @scala.annotation.tailrec
    def mk(games: NonEmptyList[Game], parsedMoves: List[ParsedMove]): (NonEmptyList[Game], Option[String]) =
      parsedMoves match {
        case Nil => (games, None)
        case parsedMove :: rest =>
          games.head(parsedMove) match {
            case Valid(newGame) => mk(newGame :: games, rest)
            case Invalid(err)   => (games, err.some)
          }
      }

    mk(NonEmptyList.one(makeGame(initialSfen, variant)), parsedMoves.toList) match {
      case (games, err) => (games.reverse, err)
    }
  }

  def gamesWhileValid(
      parsedMoves: Seq[ParsedMove],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): (NonEmptyList[Game], Option[String]) = gamesWhileValid(parsedMoves.toList, initialSfen, variant)

  def gamesWhileValid(
      parsedMoves: Vector[ParsedMove],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): (NonEmptyList[Game], Option[String]) = gamesWhileValid(parsedMoves.toList, initialSfen, variant)

  def situations(
      usis: List[Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): Validated[String, NonEmptyList[Situation]] = {
    val init = initialSfenToSituation(initialSfen, variant)
    situations(usis, init)
  }

  def situations(
      usis: List[Usi],
      situation: Situation
  ): Validated[String, NonEmptyList[Situation]] =
    usis.foldLeft[Validated[String, NonEmptyList[Situation]]](valid(NonEmptyList.one(situation))) {
      case (acc, usi) =>
        acc andThen { sits =>
          sits.head(usi) andThen { sit =>
            valid(sit :: sits)
          }
        }
    } map (_.reverse)

  def situations(
      usis: Seq[Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): Validated[String, NonEmptyList[Situation]] = situations(usis.toList, initialSfen, variant)

  def situations(
      usis: Vector[Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): Validated[String, NonEmptyList[Situation]] = situations(usis.toList, initialSfen, variant)

  def situations(
      moves: Seq[Move],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): NonEmptyList[Situation] = {
    val init = initialSfenToSituation(initialSfen, variant)
    situations(init, moves)
  }

  def situations(
      situation: Situation,
      moves: List[Move]
  ): NonEmptyList[Situation] = moves
    .foldLeft[NonEmptyList[Situation]](NonEmptyList.one(situation)) { (acc, move) => acc.head(move) :: acc }
    .reverse

  def situations(
      situation: Situation,
      moves: Seq[Move]
  ): NonEmptyList[Situation] = situations(situation, moves.toList)

  def situations(
      situation: Situation,
      moves: Vector[Move]
  ): NonEmptyList[Situation] = situations(situation, moves.toList)

  // TODO: remove backward compatibility code
  def usiWithRoleWhilePossible(
      usis: Usi.Moves,
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): List[Move] = Usi.Moves(usis, initialSfen, variant) toList

  def plyAtSfen(
      parsedMoves: List[ParsedMove],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
      atSfen: Sfen
  ): Validated[String, Int] =
    if (atSfen.toSituation(variant).isEmpty) invalid(s"Invalid Sfen $atSfen")
    else {
      @scala.annotation.tailrec
      def recursivePlyAtSfen(
          sit: Situation,
          parsedMoves: List[ParsedMove],
          ply: Int
      ): Validated[String, Int] =
        parsedMoves match {
          case Nil => invalid(s"Can't find $atSfen, reached ply $ply")
          case parsedMove :: rest =>
            sit(parsedMove) match {
              case Valid(sitAfter) =>
                if (sitAfter.toSfen.truncate == atSfen.truncate) valid(ply)
                else recursivePlyAtSfen(sitAfter, rest, ply + 1)
              case Invalid(err) => invalid(s"Failed plyAtSfen with: $err")
            }
        }

      val sit = initialSfenToSituation(initialSfen, variant)
      recursivePlyAtSfen(sit, parsedMoves.toList, initialSfen.flatMap(_.moveNumber) | 1)
    }

  private def initialSfenToSituation(initialSfen: Option[Sfen], variant: shogi.variant.Variant): Situation =
    initialSfen.flatMap(_.toSituation(variant)) | Situation(variant)

  private def makeGame(initialSfen: Option[Sfen], variant: shogi.variant.Variant): Game =
    Game(variant.some, initialSfen)
}
