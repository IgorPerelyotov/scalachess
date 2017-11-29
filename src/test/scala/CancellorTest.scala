package chess

import StdBoard._

class CancellorTest extends ChessTest {

  "a cancellor" should {

    val cancellor = White - Cancellor

    "move in any of 8 positions, 2 and 1 squares away" in {
      pieceMoves(cancellor, E4) must bePoss(E5, E6, E7, E8, E3, E2, E1, F4, G4, H4, D4, C4, B4, A4, F6, G5, G3, F2, D2, C3, C5, D6)
    }

    "move in any of 8 positions, 2 and 1 squares away, even when at the edges" in {
      pieceMoves(cancellor, H8) must bePoss(H7, H6, H5, H4, H3, H2, H1, G8, F8, E8, D8, C8, B8, A8, F7, G6)
    }

    "not move to positions that are occupied by the same colour" in {
      val board = """
k B

   B
    P
  C
    P
PPPP PPP
 NBQKBNR
"""
      val dests = board destsFrom C4

      board destsFrom C4 must bePoss(board, """
k B
  x
 xxB
x x P
xxCxxxxx
x x P
PPPP PPP
 NBQKBNR
""")
    }

    "capture opponent pieces" in {
      val board = """
k B
     q
p ppp

N C    P
  p
PPPPPPPP
 NBQKBNR
"""
      board destsFrom C4 must bePoss(board, """
k B
     q
pxxxp
x x x
NxCxxxxP
x x x
PPPPPPPP
 NBQKBNR
""")
    }

    "threaten" in {
      val board = """
k B
  q  q
p  pp

N C    P

PPPPPPPP
 NBQKBNR
"""
      "a reachable enemy on line" in {
        board actorAt C4 map (_ threatens C7) must beSome(true)
      }
      "a reachable enemy on knight's way" in {
        board actorAt C4 map (_ threatens D6) must beSome(true)
      }
      "an unreachable enemy" in {
        board actorAt C4 map (_ threatens A8) must beSome(false)
      }
      "a reachable friend on line" in {
        board actorAt C4 map (_ threatens C2) must beSome(true)
      }
      "a reachable friend on knight's way" in {
        board actorAt C4 map (_ threatens B2) must beSome(true)
      }
      "nothing up" in {
        board actorAt C4 map (_ threatens D4) must beSome(true)
      }
      "nothing down" in {
        board actorAt C4 map (_ threatens B4) must beSome(true)
      }
    }
  }
}
