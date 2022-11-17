package shogi

class DividerTest extends ShogiTest {

  val usis: Usis = shogi.format.usi.Usi.read(format.usi.Fixtures.fromProd2).get

  // more tests wanted
  "the divider finds middlegame and endgame" should {
    "fromProd2" in {
      val situations = Replay.situations(usis, None, variant.Standard).toOption.get
      val divided    = Divider(situations.toList)
      divided.middle must beSome.like { _ must beBetween(10, 30) }
      divided.end must beNone
    }
  }
}
