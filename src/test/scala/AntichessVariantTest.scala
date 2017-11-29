package chess

import scalaz.Validation.FlatMap._
import variant.Antichess
import format.Forsyth
import format.pgn.Reader

class AntichessVariantTest extends ChessTest {

  // Random PGN taken from FICS
  val fullGame = """[Event "3 0 rated antichess"]
[Site "freechess.org"]
[Date "2014.12.12"]
[Round "?"]
[White "Gnarus"]
[Black "CatNail"]
[Result "0-1"]
[ResultDescription "CatNail wins by losing all material"]
[WhiteElo "1778"]
[BlackElo "2025"]
[PlyCount "67"]
[Variant "antichess"]
[TimeControl "180+0"]
[WhiteClock "00:03:00.0"]
[BlackClock "00:03:00.0"]
[WhiteLagMillis "1470"]
[BlackLagMillis "0"]
[WhiteRemainingMillis "139297"]
[BlackRemainingMillis "173163"]
[WhiteOnTop "0"]

1. g3 {[%emt 0.000]} h6 {[%emt 0.000]} 2. Nh3 {[%emt 1.156]} a6 {[%emt 0.221]} 3. Ng5 {[%emt 0.454]}
hxg5 {[%emt 0.220]} 4. Bh3 {[%emt 0.671]} Rxh3 {[%emt 0.221]} 5. Rg1 {[%emt 0.516]}
Rxg3 {[%emt 0.201]} 6. hxg3 {[%emt 0.578]} b6 {[%emt 0.201]} 7. Na3 {[%emt 1.204]}
g6 {[%emt 0.225]} 8. Nb5 {[%emt 0.436]} axb5 {[%emt 0.219]} 9. a4 {[%emt 0.735]}
Rxa4 {[%emt 0.206]} 10. Rxa4 {[%emt 0.875]} bxa4 {[%emt 0.221]} 11. b3 {[%emt 0.296]}
axb3 {[%emt 0.201]} 12. cxb3 {[%emt 0.109]} Nh6 {[%emt 0.200]} 13. Ba3 {[%emt 0.656]}
Na6 {[%emt 0.221]} 14. Bxe7 {[%emt 0.703]} Bxe7 {[%emt 0.223]} 15. b4 {[%emt 0.656]}
Bxb4 {[%emt 0.200]} 16. g4 {[%emt 5.594]} Bxd2 {[%emt 0.201]} 17. Qxd2 {[%emt 0.906]}
Nxg4 {[%emt 0.199]} 18. Qxg5 {[%emt 0.907]} Nxf2 {[%emt 0.221]} 19. Qxd8 {[%emt 4.313]}
Kxd8 {[%emt 0.220]} 20. Rxg6 {[%emt 1.718]} fxg6 {[%emt 0.221]} 21. Kxf2 {[%emt 0.767]}
c5 {[%emt 0.200]} 22. Kf3 {[%emt 1.594]} Ke8 {[%emt 0.201]} 23. e4 {[%emt 0.939]}
Nb8 {[%emt 0.201]} 24. e5 {[%emt 0.484]} d5 {[%emt 0.201]} 25. exd6 {[%emt 0.437]}
Nd7 {[%emt 0.201]} 26. Ke3 {[%emt 3.984]} Ke7 {[%emt 0.201]} 27. dxe7 {[%emt 1.422]}
g5 {[%emt 0.200]} 28. Kd4 {[%emt 1.172]} cxd4 {[%emt 0.200]} 29. e8=R {[%emt 4.329]}
Ne5 {[%emt 0.364]} 30. Rxc8 {[%emt 0.469]} Nc4 {[%emt 0.201]} 31. Rxc4 {[%emt 1.592]}
b5 {[%emt 0.223]} 32. Rxd4 {[%emt 0.359]} b4 {[%emt 0.202]} 33. Rxb4 {[%emt 0.500]}
g4 {[%emt 0.200]} 34. Rxg4 {[%emt 0.172]} 0-1"""

  "Antichess " should {

    "Allow an opening move for white taking into account a player may move without taking if possible" in {
      val startingPosition = Game(Antichess)
      val afterFirstMove = startingPosition.playMove(StdBoard.E2, StdBoard.E4, None)

      afterFirstMove must beSuccess.like {
        case newGame =>
          val fen = Forsyth >> newGame
          fen mustEqual "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b - - 0 1"
      }
    }

    "Not allow a player to make a non capturing move if a capturing move is available" in {
      val game = Game(Antichess)
      val gameAfterOpening = game.playMoves((StdBoard.E2, StdBoard.E4), (StdBoard.F7, StdBoard.F5))

      val invalidGame = gameAfterOpening flatMap (_.playMove(StdBoard.H2, StdBoard.H4))

      invalidGame must beFailure.like {
        case failMsg => failMsg mustEqual scalaz.NonEmptyList("Piece on h2 cannot move to h4")
      }
    }

    "A situation in antichess should only present the capturing moves if the player can capture" in {
      val game = Game(Antichess)
      val gameAfterOpening = game.playMoves((StdBoard.E2, StdBoard.E4), (StdBoard.F7, StdBoard.F5))

      gameAfterOpening must beSuccess.like {
        case newGame =>
          newGame.situation.moves.size must beEqualTo(1)
          newGame.situation.moves.values.find(_.find(_.captures == false).nonEmpty) must beNone
      }

    }

    "Allow a capturing move to be made" in {
      val game = Game(Antichess).playMoves((StdBoard.E2, StdBoard.E4), (StdBoard.F7, StdBoard.F5), (StdBoard.E4, StdBoard.F5))
      game must beSuccess
    }

    "Not permit a player to castle" in {
      // Castling is not allowed in antichess
      val game = Game(Antichess).playMoves(
        (StdBoard.E2, StdBoard.E4),
        (StdBoard.E7, StdBoard.E5),
        (StdBoard.F1, StdBoard.E2),
        (StdBoard.G8, StdBoard.H6),
        (StdBoard.G1, StdBoard.H3)
      )

      val possibleDestinations = game flatMap (_.board.destsFrom(StdBoard.E1).toValid("king has no destinations"))

      possibleDestinations must beSuccess.like {
        case dests =>
          // G1 (to castle) should not be a valid destination
          dests must beEqualTo(List(StdBoard.F1))
      }

    }

    "Not allow a king to be put into check" in {
      val game = Game(Antichess).playMoves(
        StdBoard.E2 -> StdBoard.E4,
        StdBoard.E7 -> StdBoard.E5,
        StdBoard.D1 -> StdBoard.H5
      )

      game must beSuccess.like {
        case newGame =>
          newGame.situation.check must beFalse
      }
    }

    "Allow kings to be captured" in {
      val game = Game(Antichess).playMoves(
        StdBoard.E2 -> StdBoard.E4,
        StdBoard.E7 -> StdBoard.E5,
        StdBoard.D1 -> StdBoard.H5,
        StdBoard.F7 -> StdBoard.F6,
        StdBoard.H5 -> StdBoard.E8
      )

      game must beSuccess.like {
        case newGame =>
          newGame.board.kingPosOf(Color.black) must beNone
      }
    }

    "Not allow a king to be check mated" in {
      val game = Game(Antichess).playMoves(
        StdBoard.F2 -> StdBoard.F3,
        StdBoard.E7 -> StdBoard.E6,
        StdBoard.G2 -> StdBoard.G4,
        StdBoard.D8 -> StdBoard.H4
      )

      game must beSuccess.like {
        case newGame =>
          newGame.situation.checkMate must beFalse
      }
    }

    "Allow a pawn to be promoted to a king" in {
      val positionString = "8/5P2/8/2b5/8/8/4B3/8 w - -"
      val originalGame = fenToGame(positionString, Antichess)

      val newGame = originalGame flatMap (_.apply(StdBoard.F7, StdBoard.F8, Some(King))) map (_._1)

      newGame must beSuccess.like {
        case gameWithPromotion =>
          gameWithPromotion.board(StdBoard.F8).mustEqual(Some(White - King))
      }

    }

    "Be drawn when there are only opposite colour bishops remaining" in {
      val positionString = "8/2b5/8/8/8/6Q1/4B3/8 b - -"
      val originalGame = fenToGame(positionString, Antichess)

      val newGame = originalGame flatMap (_.apply(StdBoard.C7, StdBoard.G3, None)) map (_._1)

      newGame must beSuccess.like {
        case drawnGame =>
          drawnGame.situation.end must beTrue
          drawnGame.situation.autoDraw must beTrue
          drawnGame.situation.winner must beNone
          drawnGame.situation.status must beSome.like {
            case status => status == Status.Draw
          }
      }
    }

    "Be drawn on multiple bishops on the opposite color" in {
      val positionString = "8/6P1/8/8/1b6/8/8/5B2 w - -"
      val originalGame = fenToGame(positionString, Antichess)

      val newGame = originalGame flatMap (_.apply(StdBoard.G7, StdBoard.G8, Bishop.some)) map (_._1)

      newGame must beSuccess.like {
        case drawnGame =>
          drawnGame.situation.end must beTrue
          drawnGame.situation.autoDraw must beTrue
          drawnGame.situation.winner must beNone
          drawnGame.situation.status must beSome.like {
            case status => status == Status.Draw
          }
      }

    }

    "Not be drawn when the black and white bishops are on the same coloured squares " in {
      val position = "7b/8/1p6/8/8/8/5B2/8 w - -"
      val originalGame = fenToGame(position, Antichess)

      val newGame = originalGame flatMap (_.apply(StdBoard.F2, StdBoard.B6, None)) map (_._1)

      newGame must beSuccess.like {
        case nonDrawnGame =>
          nonDrawnGame.situation.end must beFalse
          nonDrawnGame.situation.autoDraw must beFalse
          nonDrawnGame.situation.winner must beNone
      }
    }

    "Be drawn when there are only opposite colour bishops and pawns which could not attack those bishops remaining" in {
      val position = "8/6p1/4B1P1/4p3/4P3/8/2p5/8 b - - 1 28"
      val originalGame = fenToGame(position, Antichess)

      val newGame = originalGame flatMap (_.apply(StdBoard.C2, StdBoard.C1, Some(Bishop))) map (_._1)

      newGame must beSuccess.like {
        case drawnGame =>
          drawnGame.situation.end must beTrue
          drawnGame.situation.autoDraw must beTrue
          drawnGame.situation.status must beSome.like {
            case status => status == Status.Draw
          }
      }
    }

    "Not be drawn on opposite color bishops but with pawns that could be forced to attack a bishop" in {
      val position = "8/6p1/1B4P1/4p3/4P3/8/3p4/8 b - -"
      val originalGame = fenToGame(position, Antichess)

      val newGame = originalGame flatMap (_.apply(StdBoard.D2, StdBoard.D1, Some(Bishop))) map (_._1)

      newGame must beSuccess.like {
        case nonDrawnGame =>
          nonDrawnGame.situation.end must beFalse
          nonDrawnGame.situation.autoDraw must beFalse
          nonDrawnGame.situation.status must beNone
      }
    }

    "Not be drawn where a white bishop can attack a black pawn in an almost closed position" in {
      val position = "5b2/1P4p1/4B1P1/4p3/4P3/8/8/8 w - -"
      val originalGame = fenToGame(position, Antichess)

      val newGame = originalGame flatMap (_.apply(StdBoard.B7, StdBoard.B8, Bishop.some)) map (_._1)

      newGame must beSuccess.like {
        case nonDrawnGame =>
          nonDrawnGame.situation.end must beFalse
          nonDrawnGame.situation.autoDraw must beFalse
          nonDrawnGame.situation.status must beNone
      }

    }

    "Not be drawn where a pawn is unattackable, but is blocked by a bishop, not a pawn" in {
      val position = "8/8/4BbP1/4p3/4P3/8/8/8 b - -"
      val originalGame = fenToGame(position, Antichess)

      val newGame = originalGame flatMap (_.playMoves(StdBoard.F6 -> StdBoard.G7))

      newGame must beSuccess.like {
        case nonDrawnGame =>
          nonDrawnGame.situation.end must beFalse
          nonDrawnGame.situation.autoDraw must beFalse
          nonDrawnGame.situation.status must beNone
      }
    }

    "Not be drawn on insufficient mating material" in {
      val positionString = "4K3/8/1b6/8/8/8/5B2/3k4 b - -"
      val maybeGame = fenToGame(positionString, Antichess)

      maybeGame must beSuccess.like {
        case game =>
          game.situation.end must beFalse
      }
    }

    "Be drawn on a three move repetition" in {
      val game = Game(Antichess)

      val moves = List((StdBoard.G1, StdBoard.F3), (StdBoard.G8, StdBoard.F6), (StdBoard.F3, StdBoard.G1), (StdBoard.F6, StdBoard.G8))
      val repeatedMoves = List.fill(3)(moves).flatten

      val drawnGame = game.playMoveList(repeatedMoves)

      drawnGame must beSuccess.like {
        case g => g.situation.threefoldRepetition must beTrue
      }

    }

    "Successfully play through a full game until one player loses all their pieces" in {
      val game = Reader.full(fullGame)

      game must beSuccess.like {
        case Reader.Result.Complete(replay) =>
          val game = replay.state

          game.situation.end must beTrue

          // In antichess, the player who has just lost all their pieces is the winner
          game.situation.winner must beSome.like {
            case color =>
              color == Black;
          }
      }
    }

    "Win on a traditional stalemate where the player has no valid moves" in {
      val positionString = "8/p7/8/P7/8/8/8/8 w - -"
      val maybeGame = fenToGame(positionString, Antichess)

      val drawnGame = maybeGame flatMap (_.playMoves((StdBoard.A5, StdBoard.A6)))

      drawnGame must beSuccess.like {
        case game =>
          game.situation.end must beTrue
          game.situation.winner must beSome.like {
            case color =>
              color == Black;
          }
      }
    }

    "Stalemate is a win - second test" in {
      val fen = "2Q5/8/p7/8/8/8/6PR/8 w - -"
      val maybeGame = fenToGame(fen, Antichess)

      val drawnGame = maybeGame flatMap (_.playMoves((StdBoard.C8, StdBoard.A6)))

      drawnGame must beSuccess.like {
        case game =>
          game.situation.end must beTrue
          game.situation.status must beSome.like {
            case state => state == Status.VariantEnd

          }
          game.situation.winner must beSome.like {
            case color =>
              color == Black;
          }
      }
    }

  }

}
