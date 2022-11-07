package shogi

import format.ParsedMove

import format.usi.Usi

class ReplayPerfTest extends ShogiTest {

  // args(skipAll = true)

  val nb                                  = 100
  val usis: List[List[Usi]]               = format.usi.Fixtures.prod500standard.map(Usi.readList(_).get)
  val parsedMoves: List[List[ParsedMove]] = usis map parseTrustedUsis
  val iterations                          = 10

  def runOne(parsedMoves: List[ParsedMove]) =
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
