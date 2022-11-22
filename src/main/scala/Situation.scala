package shogi

import cats.data.Validated
import cats.implicits._

import shogi.format.forsyth.Sfen
import shogi.format.ParsedMove
import shogi.format.psn.PsnMove
import shogi.format.usi.Usi
import shogi.variant.Variant

case class Situation(
    board: Board,
    hands: Hands,
    color: Color,
    history: History,
    variant: Variant
) {

  def apply(move: Move): Situation =
    move match {
      case move: PieceMove => variant.applyMove(this, move)
      case drop: PieceDrop => variant.applyDrop(this, drop)
    }

  def apply(usi: Usi): Validated[String, Situation] = {
    val move: Validated[String, Move] = usi match {
      case Usi.Move(orig, dest, promotion) =>
        Validated.fromOption(board(orig), s"No piece at $orig") map {
          PieceMove(board, _, orig, dest, promotion)
        }
      case Usi.Drop(role, dest) =>
        if (variant.handRoles contains role) Validated.valid(PieceDrop(Piece(color, role), dest))
        else Validated.invalid(s"$color cannot drop $role in this $variant situation")
    }
    move map apply
  }

  def validate(psn: PsnMove): Validated[String, Move] = psn match {
    case shogi.format.psn.Move(dest, role, _, file, rank, promotion, _) =>
      board.pieces.foldLeft(none[PieceMove]) {
        case (None, (pos, piece))
            if piece.color == color && piece.role == role &&
              file.fold(true)(pos.file.index + 1 == _) &&
              rank.fold(true)(pos.rank.index + 1 == _) &&
              piece.eyes(pos, dest) =>
          moveActorAt(pos) map { a =>
            (if (promotion) a.promotionMoves(this) else a.unpromotionMoves(this)).filter {
              _.dest == dest
            }.head
          }
        case (m, _) => m
      } match {
        case None       => Validated invalid s"Move $role $file$rank not found:\n$this"
        case Some(move) => Validated valid move
      }
    case shogi.format.psn.Drop(role, dest, _) => {
      if (variant.handRoles contains role) {
        val piece = Piece(color, role)
        if (dropDestsOf(piece) contains dest) Validated.valid(PieceDrop(piece, dest))
        else Validated.invalid(s"No $role drop at $dest found:\n$this")
      } else Validated.invalid(s"$color cannot drop $role in this $variant situation")
    }
  }

  def apply(parsedMove: ParsedMove): Validated[String, Situation] = apply(parsedMove.usi)

  // Moves

  lazy val moveActors: Map[Pos, MoveActor] = board.pieces map { case (pos, piece) =>
    (pos, MoveActor(piece, pos, this))
  }

  def moveActorAt(at: Pos): Option[MoveActor] = moveActors get at

  def moveDestsFrom(from: Pos): Option[List[Pos]] =
    moveActorAt(from) map (_.destinations)

  def moveActorsOf(c: Color): List[MoveActor] = board.pieces.collect {
    case (pos, piece) if piece.color == c => MoveActor(piece, pos, this)
  }.toList

  def moveDestinations: Map[Pos, List[Pos]] =
    moveActorsOf(color).collect {
      case actor if actor.destinations.nonEmpty => actor.pos -> actor.destinations
    }.toMap

  lazy val hasMoveDestinations: Boolean =
    moveActorsOf(color).exists(_.destinations.nonEmpty)

  // Drops

  def dropActorOf(piece: Piece): Option[DropActor] =
    hands.rolesOf(piece.color) collectFirst { case piece.role => DropActor(piece, this) }

  def dropActorsOf(c: Color): List[DropActor] =
    hands.rolesOf(c) map { role => DropActor(Piece(c, role), this) }

  def dropDestinations: Map[Piece, List[Pos]] =
    dropActorsOf(color).collect {
      case actor if actor.destinations.nonEmpty => actor.piece -> actor.destinations
    }.toMap

  def hasDropDestinations: Boolean =
    dropActorsOf(color).exists(_.destinations.nonEmpty)

  def dropDestsOf(piece: Piece): List[Pos] = dropActorOf(piece).fold[List[Pos]](Nil)(_.destinations)

  // King safety

  def check: Boolean = checkOf(color)

  private def checkOf(c: Color): Boolean = c.fold(checkSente, checkGote)

  lazy val checkSente = variant.check(board, Sente)
  lazy val checkGote  = variant.check(board, Gote)

  def checkSquares = variant checkSquares this

  // Not taking into account specific drop rules
  lazy val possibleDropDests: List[Pos] =
    if (check) board.kingPosOf(color).fold[List[Pos]](Nil)(DropActor.blockades(this, _))
    else variant.allPositions.filterNot(board.pieces contains _)

  // Results

  def checkmate: Boolean = variant checkmate this

  def stalemate: Boolean = variant stalemate this

  def perpetualCheck: Boolean = variant perpetualCheck this

  def autoDraw: Boolean =
    (history.fourfoldRepetition && !perpetualCheck) ||
      variant.specialDraw(this) ||
      variant.isInsufficientMaterial(this)

  def opponentHasInsufficientMaterial: Boolean = variant opponentHasInsufficientMaterial this

  def variantEnd = variant specialEnd this

  def impasse = variant impasse this

  def end(withImpasse: Boolean): Boolean =
    checkmate || stalemate || autoDraw || perpetualCheck || variantEnd || (withImpasse && impasse)

  def winner: Option[Color] = variant.winner(this)

  def materialImbalance: Int = variant.materialImbalance(this)

  def valid(strict: Boolean) = variant.valid(this, strict)

  def playable(strict: Boolean, withImpasse: Boolean): Boolean =
    valid(strict) && !end(withImpasse) && !copy(color = !color).check

  lazy val status: Option[Status] =
    if (checkmate) Status.Mate.some
    else if (variantEnd) Status.VariantEnd.some
    else if (stalemate) Status.Stalemate.some
    else if (impasse) Status.Impasse27.some
    else if (perpetualCheck) Status.PerpetualCheck.some
    else if (autoDraw) Status.Draw.some
    else none

  // Util

  def withBoard(board: Board)                     = copy(board = board)
  def withHands(hands: Hands)                     = copy(hands = hands)
  def withHistory(history: History)               = copy(history = history)
  def withVariant(variant: shogi.variant.Variant) = copy(variant = variant)

  def switch = copy(color = !color)

  def visual: String = format.forsyth.Visual render this

  def toSfen: Sfen = Sfen(this)

  override def toString = s"${variant.name}\n$visual\nLast Move: ${history.lastMove}\n"
}

object Situation {

  def apply(variant: shogi.variant.Variant): Situation =
    Situation(
      Board(variant),
      Hands(variant),
      Sente,
      History.empty,
      variant
    )

  def apply(board: Board, hands: Hands, color: Color, variant: Variant): Situation =
    Situation(board, hands, color, History.empty, variant)
}
