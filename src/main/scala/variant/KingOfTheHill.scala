package chess
package variant

case object KingOfTheHill extends Variant(
  id = 4,
  key = "kingOfTheHill",
  name = "King of the Hill",
  shortName = "KotH",
  title = "Bring your King to the center to win the game.",
  standardInitialPosition = true,
  boardType = StdBoard
) {

  def pieces = Standard.pieces

  private val center = Set(StdBoard.D4, StdBoard.D5, StdBoard.E4, StdBoard.E5)

  override def specialEnd(situation: Situation) =
    situation.board.kingPosOf(!situation.color) exists center.contains

  /**
   * You only need a king to be able to win in this variant
   */
  override def insufficientWinningMaterial(board: Board, color: Color) = false

  override def insufficientWinningMaterial(board: Board) = false
}

