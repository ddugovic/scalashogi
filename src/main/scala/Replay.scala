package shogi

import cats.data.Validated
import cats.data.Validated.{ invalid, valid, Invalid, Valid }
import cats.data.NonEmptyList
import cats.implicits._

import shogi.format.{ ParsedMove, ParsedMoves, Reader, Tag, Tags }
import shogi.format.forsyth.Sfen

case class Replay(setup: Game, state: Game) {
  def apply(game: Game) = copy(state = game)

  def apply(move: Move) = copy(state = state(move))
}

object Replay {

  def apply(game: Game) = new Replay(game, game)

  def apply(
      usis: List[shogi.format.usi.Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): Reader.Result =
    Reader.fromUsi(
      usis,
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
      parsedMoves: ParsedMoves,
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

    mk(NonEmptyList.one(makeGame(initialSfen, variant)), parsedMoves) match {
      case (games, err) => (games.reverse, err)
    }
  }

  def gamesWhileValid(
      moves: shogi.format.usi.Usi.Moves,
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): (NonEmptyList[Game], Option[String]) = gamesWhileValid(moves.usiMoveList, initialSfen, variant)

  def gamesWhileValid(
      usis: List[shogi.format.usi.Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): (NonEmptyList[Game], Option[String]) = {

    @scala.annotation.tailrec
    def mk(
        games: NonEmptyList[Game],
        usis: List[shogi.format.usi.Usi]
    ): (NonEmptyList[Game], Option[String]) =
      usis match {
        case Nil => (games, None)
        case usi :: rest =>
          games.head(usi) match {
            case Valid(newGame) => mk(newGame :: games, rest)
            case Invalid(err)   => (games, err.some)
          }
      }

    mk(NonEmptyList.one(makeGame(initialSfen, variant)), usis) match {
      case (games, err) => (games.reverse, err)
    }
  }

  def situations(
      usis: List[shogi.format.usi.Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): Validated[String, NonEmptyList[Situation]] = {
    val init = initialSfenToSituation(initialSfen, variant)
    situations(usis, init)
  }

  def situations(
      usis: List[shogi.format.usi.Usi],
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
      usis: Seq[shogi.format.usi.Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): Validated[String, NonEmptyList[Situation]] = situations(usis.toList, initialSfen, variant)

  def situations(
      usis: Vector[shogi.format.usi.Usi],
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

  // TODO: rename variable usis to moves
  def situations(
      usis: shogi.format.usi.Usi.Moves,
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): NonEmptyList[Situation] = {
    val init = initialSfenToSituation(initialSfen, variant)
    situations(init, usis)
  }

  def situations(
      situation: Situation,
      moves: shogi.format.usi.Usi.Moves
  ): NonEmptyList[Situation] = moves
    .foldLeft[NonEmptyList[Situation]](NonEmptyList.one(situation)) { (acc, move) => acc.head(move) :: acc }
    .reverse

  // TODO: remove backward compatibility code
  def usiWithRoleWhilePossible(
      usis: shogi.format.usi.Usi.Moves,
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): List[Move] = shogi.format.usi.Usi.Moves(usis, initialSfen, variant) toList

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
      recursivePlyAtSfen(sit, parsedMoves, initialSfen.flatMap(_.moveNumber) | 1)
    }

  private def initialSfenToSituation(initialSfen: Option[Sfen], variant: shogi.variant.Variant): Situation =
    initialSfen.flatMap(_.toSituation(variant)) | Situation(variant)

  private def makeGame(initialSfen: Option[Sfen], variant: shogi.variant.Variant): Game =
    Game(variant.some, initialSfen)
}
