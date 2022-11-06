package shogi
package variant

import scala.annotation.nowarn

import shogi.format.forsyth.Sfen

// Correctness depends on singletons for each variant ID
abstract class Variant private[variant] (
    val id: Int,
    val key: String,
    val name: String,
    val shortName: String,
    val title: String
) {

  def initialSfen: Sfen

  def numberOfRanks: Int
  def numberOfFiles: Int

  def allPositions: List[Pos]

  def pieces: PieceMap
  def hands: HandsMap

  // Roles available in the variant
  def allRoles: List[Role]

  // Only these roles will be stored in hand, order matters for export
  def handRoles: List[Role]

  // Promotions based on current variant, None for roles that do not promote
  def promote(role: Role): Option[Role]

  // Unpromotions based on current variant, for example for unpromoting pieces to hand
  def unpromote(role: Role): Option[Role]

  // Furthest rank for color
  def backrank(color: Color): Rank

  // Ranks where pieces of color can promote
  def promotionRanks(color: Color): List[Rank]

  // True if piece will never be able to move from pos
  // Used both for drops and moves without promotion
  def pieceInDeadZone(piece: Piece, pos: Pos): Boolean =
    piece.role match {
      case Pawn | Knight | Lance if backrank(piece.color) == pos.rank            => true
      case Knight if math.abs(backrank(piece.color).index - pos.rank.index) == 1 => true
      case _                                                                     => false
    }

  def canPromote(piece: Piece, orig: Pos, dest: Pos): Boolean =
    promote(piece.role).isDefined &&
      promotionRanks(piece.color).exists(r => r == dest.rank || r == orig.rank)

  def supportsDrops = true

  def addCapturedPiecesToHand = true

  def isInsideBoard(pos: Pos): Boolean =
    pos.file.index < numberOfFiles && pos.rank.index < numberOfRanks

  // Optimized for performance
  // Exists a piece of color on board, which passes the filter and attacks pos
  def posThreatened(board: Board, color: Color, pos: Pos, filter: Piece => Boolean = _ => true): Boolean =
    board.pieces exists {
      case (from, piece) if piece.color == color && filter(piece) && piece.eyes(from, pos) =>
        piece.projectionDirs.isEmpty || piece.directDirs.exists(_(from).contains(pos)) ||
          piece.role.dir(from, pos).exists {
            longRangeThreatens(board, from, _, pos)
          }
      case _ => false
    }

  // Filters out moves that would put the king in danger
  // Critical function - optimize for performance
  def kingSafetyFilter(a: MoveActor): List[Pos] = {
    // only long range roles, since you can only unpin a check from a role with projection
    val filter: Piece => Boolean =
      if ((a.piece is King) || a.situation.check) (_ => true) else (_.projectionDirs.nonEmpty)
    val stableKingPos = if (a.piece is King) None else a.situation.board kingPosOf a.color
    a.unsafeDestinations filterNot { dest =>
      (stableKingPos orElse Option.when(a.piece is King)(dest)) exists {
        posThreatened(
          a.situation.board.forceMove(a.piece, a.pos, dest),
          !a.color,
          _,
          filter
        )
      }
    }
  }

  def check(board: Board, color: Color): Boolean =
    board.kingPosOf(color) exists {
      posThreatened(board, !color, _)
    }

  def checkSquares(sit: Situation): List[Pos] =
    if (sit.check) sit.board.kingPosOf(sit.color).toList else Nil

  def longRangeThreatens(board: Board, p: Pos, dir: Direction, to: Pos): Boolean =
    dir(p) exists { next =>
      (next == to || (isInsideBoard(next) && !board.pieces
        .contains(next) && longRangeThreatens(board, next, dir, to)))
    }

  // For example, can't drop a pawn on a file with another pawn of the same color
  def dropLegalityFilter(a: DropActor): List[Pos] = {
    def illegalPawn(d: Pos) =
      (a.piece is Pawn) && (
        a.situation.board.pieces.exists { case (pos, piece) =>
          a.piece == piece && pos.file == d.file
        } ||
          a.situation.board.kingPosOf(!a.situation.color).fold(false) { kingPos =>
            a.piece.eyes(d, kingPos) && a.situation
              .withBoard(a.situation.board.forcePlace(a.piece, d))
              .switch
              .checkmate
          }
      )
    a.situation.possibleDropDests.filterNot { d =>
      pieceInDeadZone(a.piece, d) || illegalPawn(d)
    }
  }

  // Finalizes situation after move or drop
  protected def finalizeSituation(beforeSit: Situation, board: Board, hands: Hands, move: Move): Situation = {
    val newSit = beforeSit.copy(board = board, hands = hands).switch
    val h = beforeSit.history.withLastMove(move).withPositionHashes {
      val basePositionHashes =
        if (beforeSit.history.positionHashes.isEmpty) Hash(beforeSit) else beforeSit.history.positionHashes
      Hash(newSit) ++ basePositionHashes
    }
    newSit.withHistory(h)
  }

  def applyMove(sit: Situation, move: PieceMove): Situation = {
    val unpromotedCapture = sit.board(move.dest).map(p => p.updateRole(unpromote) | p)
    val hands =
      unpromotedCapture
        .filter(c => handRoles.contains(c.role) && addCapturedPiecesToHand)
        .fold(sit.hands)(sit.hands store _.switch)
    val board: Board =
      if (move.promotion)
        sit.board.promote(move.orig, move.dest, promote).get
      else
        sit.board.forceMove(move.piece, move.orig, move.dest)
    finalizeSituation(sit, board, hands, move)
  }

  def applyDrop(sit: Situation, drop: PieceDrop): Situation = {
    val hands = sit.hands.take(drop.piece).get
    val board = sit.board.forcePlace(drop.piece, drop.pos)
    finalizeSituation(sit, board, hands, drop)
  }

  @nowarn
  def impasse(sit: Situation): Boolean = false

  def perpetualCheck(sit: Situation): Boolean =
    sit.check && sit.history.fourfoldRepetition

  def stalemate(sit: Situation): Boolean =
    !sit.check && !sit.hasMoveDestinations && !sit.hasDropDestinations

  def checkmate(sit: Situation): Boolean =
    sit.check && !sit.hasMoveDestinations && !sit.hasDropDestinations

  // Player wins or loses after their move
  def winner(sit: Situation): Option[Color] =
    if (sit.checkmate || sit.stalemate) Some(!sit.color)
    else if (sit.impasse || sit.perpetualCheck) Some(sit.color)
    else None

  @nowarn
  def specialEnd(sit: Situation): Boolean = false

  @nowarn
  def specialDraw(sit: Situation) = false

  // Returns the material imbalance in pawns
  def materialImbalance(sit: Situation): Int =
    sit.board.pieces.values.foldLeft(0) { case (acc, Piece(color, role)) =>
      acc + Role.valueOf(role) * color.fold(1, -1)
    } + (sit.hands(Sente).sum(Role.valueOf) - sit.hands(Gote).sum(Role.valueOf))

  // Returns true if neither player can win. The game should end immediately.
  def isInsufficientMaterial(sit: Situation) =
    ((sit.hands.size + sit.board.pieces.size) <= 2) &&
      sit.board.pieces.forall { p => p._2 is King }

  // Returns true if the other player cannot win. This is relevant when the
  // side to move times out or disconnects. Instead of losing on time,
  // the game should be drawn.
  def opponentHasInsufficientMaterial(sit: Situation) =
    (sit.hands(!sit.color).size + sit.board.piecesOf(!sit.color).size) <= 2

  protected def hasUnmovablePieces(board: Board) =
    board.pieces.exists { case (pos, piece) =>
      pieceInDeadZone(piece, pos)
    }

  protected def hasDoublePawns(board: Board, color: Color) = {
    val pawnFiles = board.pieces.collect {
      case (pos, piece) if (piece is Pawn) && (piece is color) =>
        pos.file
    }.toList
    pawnFiles.distinct.size != pawnFiles.size
  }

  protected def validBoardSide(board: Board, strict: Boolean)(color: Color) = {
    val roles = board.piecesOf(color).map(_.role)
    roles.nonEmpty && roles.forall(allRoles contains _) &&
    (!strict || {
      roles.size <= pieces.size && roles.count(_ == King) == 1
    }) &&
    !hasUnmovablePieces(board) && !hasDoublePawns(board, color) &&
    roles.count(_ == King) <= 1
  }

  protected def validHands(hands: Hands) =
    hands.roles.forall(handRoles contains _)

  def valid(sit: Situation, strict: Boolean) =
    validHands(sit.hands) && Color.all.forall(validBoardSide(sit.board, strict) _)

  def standard  = this == Standard
  def minishogi = this == Minishogi

  override def toString = s"Variant($name)"

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode: Int = id

}

object Variant {

  val all = List(
    Standard,
    Minishogi
  )

  val byId = all map { v =>
    (v.id, v)
  } toMap

  val byKey = all map { v =>
    (v.key, v)
  } toMap

  val default = Standard

  def apply(id: Int): Option[Variant]     = byId get id
  def apply(key: String): Option[Variant] = byKey get key
  def orDefault(id: Int): Variant         = apply(id) | default
  def orDefault(key: String): Variant     = apply(key) | default

  def byName(name: String): Option[Variant] =
    all find (_.name.toLowerCase == name.toLowerCase)

  def exists(id: Int): Boolean = byId contains id

  val openingSensibleVariants: Set[Variant] = Set(
    shogi.variant.Standard
  )

  val divisionSensibleVariants: Set[Variant] = Set(
    shogi.variant.Standard
  )

}
