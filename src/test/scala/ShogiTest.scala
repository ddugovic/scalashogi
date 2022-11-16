package shogi

import cats.syntax.option._
import org.specs2.matcher.Matcher
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

import format.forsyth.{ Sfen, Visual }
import format.{ ParsedMove, ParsedMoves }
import variant._

trait ShogiTest extends Specification with ValidatedMatchers {

  implicit def stringToSituation(str: String): Situation = (Visual parse str).get

  implicit def colorChanger(str: String) =
    new {

      def as(color: Color): Situation = ((Visual parse str).get).copy(color = color)
    }

  implicit def parsedMove(before: Situation, after: Situation): ParsedMove =
    after.history.lastMove.get match {
      case m: PieceMove => format.KifMove(m.dest, m.orig, before.board.pieces(m.orig).role, m.promotion)
      case d: PieceDrop => format.ParsedDrop(d.role, d.pos)
    }

  implicit def parseTrustedUsis(usis: List[format.usi.Usi]): ParsedMoves = ParsedMoves(
    // Converts NEL toList since NEL lacks sliding(2)
    // https://stackoverflow.com/a/47006446 might be cleaner
    // but zipped causes compiler warnings
    Replay
      .situations(usis, makeSituation)
      .map {
        _.toList
          .sliding(2)
          .map { pair => parsedMove(pair.head, pair.tail.head) }
          .toList
      }
      .toOption
      .get
  )

  case class RichActor(actor: MoveActor) {
    def threatens(to: Pos): Boolean =
      actor.piece.eyes(actor.pos, to) && {
        (actor.piece.projectionDirs.isEmpty) ||
        (actor.piece.directDirs.exists(_(actor.pos).contains(to))) ||
        actor.piece.role.dir(actor.pos, to).exists {
          actor.situation.variant.longRangeThreatens(actor.situation.board, actor.pos, _, to)
        }
      }
  }

  implicit def richActor(actor: MoveActor) = RichActor(actor)

  case class RichGame(game: Game) {

    def as(color: Color): Game = game.withColor(color)

    // moves can be a simple sequence
    def apply(moves: (Pos, Pos, Boolean)*): Game =
      moves.foldLeft[Game](game) { case (g, (orig, dest, prom)) =>
        g(PieceMove(g.situation, orig, dest, prom))
      }

    // moves can be any sort of IterableOnce, including List
    def apply(moves: IterableOnce[(Pos, Pos, Boolean)]): Game =
      moves.iterator.foldLeft[Game](game) { case (g, (orig, dest, prom)) =>
        g(PieceMove(g.situation, orig, dest, prom))
      }

    def playMove(
        orig: Pos,
        dest: Pos,
        promotion: Boolean = false
    ): Game =
      game.apply(PieceMove(game.situation, orig, dest, promotion))

    def playDrop(
        role: Role,
        dest: Pos
    ): Game =
      game.apply(PieceDrop(game.situation, role, dest))

    def withClock(c: Clock) = game.copy(clock = Some(c))
  }

  implicit def richGame(game: Game) = RichGame(game)

  def sfenToGame(sfen: Sfen, variant: Variant = shogi.variant.Standard) =
    sfen.toSituation(variant) toValid "Could not construct situation from SFEN" map { sit =>
      Game(variant).copy(
        situation = sit
      )
    }

  def makeSituation(pieces: (Pos, Piece)*): Situation =
    Situation(shogi.variant.Standard).withBoard(Board(pieces))

  def makeSituation: Situation = Situation(shogi.variant.Standard)

  def makeEmptySituation: Situation = Situation(shogi.variant.Standard).withBoard(Board.empty)

  def bePoss(poss: Pos*): Matcher[Option[Iterable[Pos]]] =
    beSome.like { case p =>
      sortPoss(p.toList) must_== sortPoss(poss.toList)
    }

  def makeGame: Game = Game(makeSituation)

  def bePoss(situation: Situation, visual: String): Matcher[Option[Iterable[Pos]]] =
    beSome.like { case p =>
      Visual.addNewLines(Visual.render(situation, Map(p -> 'x'))) must_== visual
    }

  def sortPoss(poss: Seq[Pos]): Seq[Pos] = poss sortBy (_.toString)

  def pieceMoves(piece: Piece, pos: Pos): Option[List[Pos]] = {
    val sit = makeEmptySituation
    sit.withBoard(sit.board.place(piece, pos).get).moveActorAt(pos) map (_.destinations)
  }
}
