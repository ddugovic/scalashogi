package shogi
package format

trait Notation {

  def moves: NotationMoves

  def tags: Tags

  def withMoves(moves: NotationMoves): Notation

  def withTags(tags: Tags): Notation

  def updatePly(ply: Int, f: NotationMove => NotationMove) = {
    val index = ply - 1
    (moves lift index).fold(this) { move =>
      withMoves(moves.updated(index, f(move)))
    }
  }

  def nbPlies = moves.size

  def updateLastPly(f: NotationMove => NotationMove) = updatePly(nbPlies, f)

  def withEvent(title: String) =
    withTags(tags + Tag(_.Event, title))

  def render: String

  override def toString = render
}

case class Initial(comments: List[String] = Nil)

object Initial {
  val empty = Initial(Nil)
}

case class NotationMove(
    moveNumber: Int,
    move: shogi.Move,
    comments: List[String] = Nil,
    glyphs: Glyphs = Glyphs.empty,
    result: Option[String] = None,
    variations: List[NotationMoves] = Nil,
    // time left for the user who made the move, after he made it
    secondsSpent: Option[Int] = None,
    // total time spent playing so far
    secondsTotal: Option[Int] = None
)

case class NotationMoves(val underlying: NotationMoves) {
  def toList: List[NotationMove]     = underlying
  def toSeq: Seq[NotationMove]       = underlying.toSeq
  def toVector: Vector[NotationMove] = underlying.toVector
}

object NotationMoves {
  implicit def apply(moves: List[NotationMove]): NotationMoves   = new NotationMoves(moves)
  implicit def apply(moves: Seq[NotationMove]): NotationMoves    = new NotationMoves(moves.toList)
  implicit def apply(moves: Vector[NotationMove]): NotationMoves = new NotationMoves(moves.toList)
  implicit def toList(moves: NotationMoves): List[NotationMove]  = moves.toList
}
