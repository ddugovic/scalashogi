package shogi
package format

// Analysis, study, or game record (imported) notation
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
) {
  // TODO: remove backward compatibility code
  def usiWithRole = move
  def usi         = move.usi
}
