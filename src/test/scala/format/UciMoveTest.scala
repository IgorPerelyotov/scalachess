package chess
package format

class UciMoveTest extends ChessTest {

  "piotr encoding" should {
    "be reflexive" in {
      val move = Uci.Move("a2g7", StdBoard).get
      Uci.Move piotr (move.piotr, StdBoard) must_== move.some
    }
  }
}
