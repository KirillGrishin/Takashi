package object Takashi {

  type Pos = (Int, Int)
  type Grid = Vector[Vector[Int]]

  /**
   * Finds all neighbours inside the grid where we can move according to possible moves
   * @param pos Position for which to find neighbours
   * @param positions All positions of the grid
   * @return
   */
  def nextPositions(pos: Pos, moves: Set[Pos => Pos], positions: Set[Pos]): Set[Pos] =
    for {
      neighbourPos <- moves map ( move => move(pos) )
      // Check if neighbour positions is inside the grid
      if positions(neighbourPos)
    } yield neighbourPos

}
