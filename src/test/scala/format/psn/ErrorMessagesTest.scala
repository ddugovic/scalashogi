package shogi
package format.psn

class ErrorMessagesTest extends ShogiTest {

  val parser = Parser.full _

  "Invalid move" should {
    "fail" in {
      val e =
        """[abc "def"]
          |
          |1. P-2f { hello world } 2. P-3d
          |3. bla
          |""".stripMargin
      parser(e) must beInvalid
    }
  }

  "Lishogi does not support null moves" should {
    "fail" in {
      val e =
        """[abc "def"]
          |
          |1. P-2f { hello world } 2. --
          |3. P-7f
          |""".stripMargin
      parser(e) must beInvalid
    }
  }

  "extra glyphs" should {
    "ignored" in {
      val e =
        """[abc "def"]
          |
          |1. P-2f { hello world } 2. P-2d????
          |3. P-7f
          |""".stripMargin
      parser(e) must beValid
    }
  }

  "invalid glyphs" should {
    "ignored" in {
      val e =
        """[abc "def"]
          |
          |1. P-2f { hello world } 2. P-2d@@
          |3. P-7f
          |""".stripMargin
      parser(e) must beValid
    }
  }

  "bad comment" should {
    "fail" in {
      val e =
        """[abc "def"]
          |
          |1. P-2f { hello world
          |2. P-7f
          |""".stripMargin
      parser(e) must beInvalid
    }
  }

  "invalid tags 1" should {
    "failed" in {
      val e =
        """|[ab "cdef]
           |
           |1. P-2f { hello world } 2. P-2d??
           |3. P-7f
           |""".stripMargin
      parser(e) must beInvalid
    }
  }

  "invalid tags 2" should {
    "failed" in {
      val e =
        """|[ab "cdef"]    [123]
           |
           |1. P-2f { hello world } 2. P-2d??
           |3. P-7f
           |""".stripMargin
      parser(e) must beInvalid
    }
  }

  "illegal promotion" should {
    "parsed" in {
      val e =
        """|[abc "def"]
           |
           |1. K-5a+
           """.stripMargin
      parser(e) must beValid
    }
  }

}
