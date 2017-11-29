package chess
package variant

case object Standard extends Variant(
  id = 1,
  key = "standard",
  name = "Standard",
  shortName = "Std",
  title = "Standard rules of chess (FIDE)",
  standardInitialPosition = true,
  boardType = StdBoard
) {

  val pieces: Map[Pos, Piece] = Variant.symmetricRank(boardType, backRank)
}
