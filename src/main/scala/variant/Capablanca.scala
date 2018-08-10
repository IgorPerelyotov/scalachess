package chess
package variant

case object Capablanca extends Variant(
  id = 11,
  key = "capablanca",
  name = "Capablanca",
  shortName = "Capa",
  title = "Variant with two additional pieces",
  standardInitialPosition = false,
  boardType = CapaBoard
) {

  override val backRank = Vector(Rook, Knight, Archbishop, Bishop, Queen, King, Bishop, Cancellor, Knight, Rook)
  val pieces: Map[Pos, Piece] = Variant.symmetricRank(boardType, backRank)
}
