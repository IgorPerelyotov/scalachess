package chess

import Pos._

class ArchbishopTest extends ChessTest {

  "a archbishop" should {

    val archbishop = White - Archbishop

    "move in any of 8 positions, 2 and 1 squares away" in {
      pieceMoves(archbishop, E4) must bePoss(F3, G2, H1, D5, C6, B7, A8, D3, C2, B1, F5, G6, H7, F6, G5, G3, F2, D2, C3, C5, D6)
    }

    "move in any of 8 positions, 2 and 1 squares away, even when at the edges" in {
      pieceMoves(archbishop, H7) must bePoss(G8, G6, F5, E4, D3, C2, B1, F8, F6, G5)
    }

    "not move to positions that are occupied by the same colour" in {
      val board = """
k B

   B
    P
  A
    P
PPP  PPP
 NBQKBNR
"""
      val dests = board destsFrom C4

      board destsFrom C4 must bePoss(board, """
k B   x
     x
xx Bx
xx xP
  A
xx xP
PPPxxPPP
 NBQKBNR
""")
    }

    "capture opponent pieces" in {
      val board = """
k B
     q
p  pp

N A    P

PPPPPPPP
 NBQKBNR
"""
      board destsFrom C4 must bePoss(board, """
k B
     q
xx xx
xx xx
N A    P
xx xx
PPPPPPPP
 NBQKBNR
""")
    }

    "threaten" in {
      val board = """
k B
  q  q
p  pp

N A    P

PPPPPPPP
 NBQKBNR
"""
      "a reachable enemy on diagonal" in {
        board actorAt C4 map (_ threatens A6) must beSome(true)
      }
      "a reachable enemy on knight's way" in {
        board actorAt C4 map (_ threatens D6) must beSome(true)
      }
      "an unreachable enemy" in {
        board actorAt C4 map (_ threatens C7) must beSome(false)
      }
      "a reachable friend on diagonal" in {
        board actorAt C4 map (_ threatens A2) must beSome(true)
      }
      "a reachable friend on knight's way" in {
        board actorAt C4 map (_ threatens B2) must beSome(true)
      }
      "nothing up left" in {
        board actorAt C4 map (_ threatens B5) must beSome(true)
      }
      "nothing down right" in {
        board actorAt C4 map (_ threatens D3) must beSome(true)
      }
    }
  }
}
