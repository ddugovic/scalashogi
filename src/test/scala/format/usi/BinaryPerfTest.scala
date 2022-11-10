package shogi
package format.usi

class BinaryPerfTest extends ShogiTest {

  // args(skipAll = true)

  val usis: List[List[Usi]]  = Fixtures.prod500standard.map(Usi.readList(_).get)
  val moves: List[Usi.Moves] = usis.map(Usi.Moves(_, variant.Standard))
  val iterations             = 15

  def runOne(moves: Usi.Moves) =
    Binary.decodeMoves(Binary.encodeMoves(moves, variant.Standard).toVector, variant.Standard, 600)
  def run(): Unit = { moves foreach runOne }

  "playing a game" should {
    "many times" in {
      println("warming up")
      run()
      println("running tests")
      val durations = for (_ <- 1 to iterations) yield {
        val start = System.currentTimeMillis
        run()
        val duration = System.currentTimeMillis - start
        println(s"${moves.size} games in $duration ms")
        duration
      }
      val nbGames    = iterations * moves.size
      val moveMicros = (1000 * durations.sum) / nbGames
      println(s"Average = $moveMicros microseconds per game")
      println(s"          ${1000000 / moveMicros} games per second")
      true === true
    }
  }
}
