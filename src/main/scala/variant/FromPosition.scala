package chess
package variant

case object FromPosition extends Variant(
  id = 3,
  key = "fromPosition",
  name = "From Position",
  shortName = "FEN",
  title = "Custom starting position",
  standardInitialPosition = false,
  //TODO: Make possible to create Capablanca variant
  boardType = StdBoard
) {

  def pieces = Standard.pieces
}
