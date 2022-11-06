package shogi

import format.ParsedMove

import format.usi.Usi

class ReplayPerfTest extends ShogiTest {

  // args(skipAll = true)

  val nb                  = 100
  val situation           = makeSituation
  val usis: Seq[Seq[Usi]] = format.usi.Fixtures.prod500standard.map(Usi.readList(_).get)
  def parsedMove(before: Situation, after: Situation): ParsedMove =
    after.history.lastMove.get match {
      case m: PieceMove => shogi.format.CsaMove(m.dest, m.orig, before.board.pieces(m.orig).role)
      case d: PieceDrop => shogi.format.ParsedDrop(d.role, d.pos)
    }
  val parsedMoves: Seq[Seq[ParsedMove]] = usis map {
    // Converts NEL toList since NEL lacks sliding(2)
    // https://stackoverflow.com/a/47006446 might be cleaner
    // but zipped causes compiler warnings
    Replay
      .situations(_, situation)
      .toList
      .sliding(2)
      .map { pair =>
        parsedMove(pair.head, pair.tail.head)
      }
      .toSeq
  }
  val iterations = 10

  def runOne(parsedMoves: Seq[ParsedMove]) =
    Replay.gamesWhileValid(parsedMoves, None, shogi.variant.Standard)
  def run() = { parsedMoves foreach runOne }

  "playing a game" should {
    "many times" in {
      println("warming up")
      run()
      println("running tests")
      val durations = for (_ <- 1 to iterations) yield {
        val start = System.currentTimeMillis
        run()
        val duration = System.currentTimeMillis - start
        println(s"$nb games in $duration ms")
        duration
      }
      val nbGames    = iterations * nb
      val moveMicros = (1000 * durations.sum) / nbGames
      println(s"Average = $moveMicros microseconds per game")
      println(s"          ${1000000 / moveMicros} games per second")
      true === true
    }
  }
}
