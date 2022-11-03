package shogi
package format
package psn

class ReaderTest extends ShogiTest {

  import Fixtures._
  import Reader.Result._

  "only raw moves" should {
    "many games" in {
      forall(raws) { (c: String) =>
        Reader.full(c) must beValid.like { case Complete(replay) =>
          replay.state.playedPlies must_== c.split(' ').length
        }
      }
    }
  }
  "tags and moves" should {
    "with inline comments" in {
      Reader.full(inlineComments) must beValid
    }
    "immortal with NAG" in {
      Reader.full(withNag) must beValid
    }
    "comments and variations" in {
      Reader.full(commentsAndVariations) must beValid
    }
  }
}
