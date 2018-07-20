package chess
package variant

case object Capablanca extends Variant(
  id = 6,
  key = "capablanca",
  name = "Capablanca",
  shortName = "Capa",
  title = "Variant with two additional pieces",
  standardInitialPosition = true,
  boardType = CapaBoard
) {
  def pieces = Standard.pieces
}
