package shogi

import cats.data.Validated
import cats.data.Validated.{ invalid, valid, Invalid, Valid }
import cats.data.NonEmptyList
import cats.implicits._

import shogi.format.{ Reader, Tag, Tags }
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
      usis: Usis,
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): Validated[String, Replay] =
    usis.foldLeft[Validated[String, Replay]](valid(Replay(makeGame(initialSfen, variant)))) {
      case (acc, usi) =>
        acc andThen { replay =>
          replay.state(usi) andThen { game =>
            valid(replay(game))
          }
        }
    }

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

    mk(NonEmptyList.one(makeGame(initialSfen, variant)), usis.toList) match {
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
      situation: Situation,
      moves: List[Move]
  ): NonEmptyList[Situation] = moves
    .foldLeft[NonEmptyList[Situation]](NonEmptyList.one(situation)) { (acc, move) => acc.head(move) :: acc }
    .reverse

  def situations(
      situation: Situation,
      moves: Moves
  ): NonEmptyList[Situation] = situations(situation, moves.toList)

  // TODO: rename variable usis to moves
  def situations(
      usis: Moves,
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): NonEmptyList[Situation] = {
    val init = initialSfenToSituation(initialSfen, variant)
    situations(init, usis)
  }

  def usiWithRoleWhilePossible(
      usis: Usis,
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): Moves = moveWhilePossible(usis, initialSfen, variant)

  def moveWhilePossible(
      usis: Usis,
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant
  ): Moves =
    situations(usis, initialSfen, variant)
      .map { _.tail.map { _.history.lastMove.get } }
      .toOption
      .get
      .toVector

  def plyAtSfen(
      moves: Moves,
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
