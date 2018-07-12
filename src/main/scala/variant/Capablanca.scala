package chess
package variant

case object Capablanca extends Variant(
  id = 6,
  key = "capablanca",
  name = "Capablanca",
  shortName = "Capa",
  title = "Variant with two additional pieces",
  standardInitialPosition = true,
  boardType = StdBoard
) {
  def pieces = Standard.pieces
}
