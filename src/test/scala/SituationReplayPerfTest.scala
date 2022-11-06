package shogi

import format.usi._

class SituationReplayPerfTest extends ShogiTest {

  // args(skipAll = true)

  val nb         = 100
  val iterations = 10

  val situation                = makeSituation
  val usis                     = format.usi.Fixtures.prod500standard.take(nb).map(Usi.readList(_).get)
  val moves                    = usis map { Replay(situation, _) }
  def runOne(moves: Seq[Move]) = Replay.situations(situation, moves)
  def run() = { moves foreach runOne }

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
