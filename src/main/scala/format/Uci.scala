package chess
package format

sealed trait Uci {

  def uci: String
  def piotr: String

  def origDest: (Pos, Pos)

  def apply(situation: Situation): Valid[MoveOrDrop]
}

object Uci
  extends scalaz.std.OptionInstances
  with scalaz.syntax.ToTraverseOps {

  case class Move(
      orig: Pos,
      dest: Pos,
      promotion: Option[PromotableRole] = None
  ) extends Uci {

    def keys = orig.key + dest.key
    def uci = keys + promotionString

    def keysPiotr = orig.piotrStr + dest.piotrStr
    def piotr = keysPiotr + promotionString

    def promotionString = promotion.fold("")(_.forsyth.toString)

    def origDest = orig -> dest

    def apply(situation: Situation) = situation.move(orig, dest, promotion) map Left.apply
  }

  object Move {

    def apply(move: String, boardType: BoardType): Option[Move] = for {
      orig ← boardType.posAt(move take 2)
      dest ← boardType.posAt(move drop 2 take 2)
      promotion = move lift 4 flatMap Role.promotable
    } yield Move(orig, dest, promotion)

    def piotr(move: String, boardType: BoardType) = for {
      orig ← move.headOption flatMap boardType.piotr
      dest ← move lift 1 flatMap boardType.piotr
      promotion = move lift 2 flatMap Role.promotable
    } yield Move(orig, dest, promotion)

    def fromStrings(origS: String, destS: String, promS: Option[String], boardType: BoardType) = for {
      orig ← boardType.posAt(origS)
      dest ← boardType.posAt(destS)
      promotion = Role promotable promS
    } yield Move(orig, dest, promotion)
  }

  case class Drop(role: Role, pos: Pos) extends Uci {

    def uci = s"${role.pgn}@${pos.key}"

    def piotr = s"${role.pgn}@${pos.piotrStr}"

    def origDest = pos -> pos

    def apply(situation: Situation) = situation.drop(role, pos) map Right.apply
  }

  object Drop {

    def fromStrings(roleS: String, posS: String, boardType: BoardType) = for {
      role ← Role.allByName get roleS
      pos ← boardType.posAt(posS)
    } yield Drop(role, pos)
  }

  case class WithSan(uci: Uci, san: String)

  def apply(move: chess.Move) = Uci.Move(move.orig, move.dest, move.promotion)

  def apply(drop: chess.Drop) = Uci.Drop(drop.piece.role, drop.pos)

  def apply(move: String, boardType: BoardType): Option[Uci] =
    if (move lift 1 contains '@') for {
      role ← move.headOption flatMap Role.allByPgn.get
      pos ← boardType.posAt(move drop 2 take 2)
    } yield Uci.Drop(role, pos)
    else Uci.Move(move, boardType)

  def piotr(move: String, boardType: BoardType): Option[Uci] =
    if (move lift 1 contains '@') for {
      role ← move.headOption flatMap Role.allByPgn.get
      pos ← move lift 2 flatMap boardType.piotr
    } yield Uci.Drop(role, pos)
    else Uci.Move.piotr(move, boardType)

  def readList(moves: String, boardType: BoardType): Option[List[Uci]] =
    moves.split(' ').toList.map(x => apply(x, boardType)).sequence

  def writeList(moves: List[Uci]): String =
    moves.map(_.uci) mkString " "

  def readListPiotr(moves: String, boardType: BoardType): Option[List[Uci]] =
    moves.split(' ').toList.map(x => piotr(x, boardType)).sequence

  def writeListPiotr(moves: List[Uci]): String =
    moves.map(_.piotr) mkString " "
}
