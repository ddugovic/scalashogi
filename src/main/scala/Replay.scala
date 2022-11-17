package shogi

import cats.data.Validated
import cats.data.Validated.{ invalid, valid, Invalid, Valid }
import cats.data.NonEmptyList
import cats.implicits._

import shogi.format.{ ParsedMove, Reader, Tag, Tags }
import shogi.format.forsyth.Sfen

case class Replay(setup: Game, state: Game) {
  def apply(game: Game) = copy(state = game)

  def apply(move: Move) = copy(state = state(move))
}

object Replay {

  def apply(game: Game) = new Replay(game, game)

  def apply(
      usis: Usis,
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
      moves: shogi.format.usi.Usi.Moves,
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): (NonEmptyList[Game], Option[String]) = gamesWhileValid(moves.toUsis, initialSfen, variant)

  def gamesWhileValid(
      usis: Usis,
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
      usis: Usis,
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): Validated[String, NonEmptyList[Situation]] = {
    val init = initialSfenToSituation(initialSfen, variant)
    situations(usis, init)
  }

  def situations(
      usis: Usis,
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

  def usiWithRoleWhilePossible(
      usis: Usis,
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): shogi.format.usi.Usi.Moves = shogi.format.usi.Usi.Moves(usis, initialSfen, variant)

  def plyAtSfen(
      moves: shogi.format.usi.Usi.Moves,
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
      atSfen: Sfen
  ): Validated[String, Int] =
    if (atSfen.toSituation(variant).isEmpty) invalid(s"Invalid Sfen $atSfen")
    else {
      @scala.annotation.tailrec
      def recursivePlyAtSfen(
          sit: Situation,
          moves: List[Move],
          ply: Int
      ): Validated[String, Int] =
        moves match {
          case Nil => invalid(s"Can't find $atSfen, reached ply $ply")
          case move :: rest => {
            val sitAfter = sit(move)
            if (sitAfter.toSfen.truncate == atSfen.truncate) valid(ply)
            else recursivePlyAtSfen(sitAfter, rest, ply + 1)
          }
        }

      val sit = initialSfenToSituation(initialSfen, variant)
      recursivePlyAtSfen(sit, moves.toList, initialSfen.flatMap(_.moveNumber) | 1)
    }

  private def initialSfenToSituation(initialSfen: Option[Sfen], variant: shogi.variant.Variant): Situation =
    initialSfen.flatMap(_.toSituation(variant)) | Situation(variant)

  private def makeGame(initialSfen: Option[Sfen], variant: shogi.variant.Variant): Game =
    Game(variant.some, initialSfen)
}
