package Takashi

/**
 * Counts all possible routes from top-left to down-right
 */
object RoutesCounter {

  // Possible moves for the first problem; defined as separate functions in case Takashi-kun learns
  // new moves in future
  def up   (pos: Pos): Pos = pos match { case (x, y) => (x, y - 1) }
  def down (pos: Pos): Pos = pos match { case (x, y) => (x, y + 1) }
  def right(pos: Pos): Pos = pos match { case (x, y) => (x + 1, y) }
  def left (pos: Pos): Pos = pos match { case (x, y) => (x - 1, y) }

  // A list of possible moves
  val possibleMoves = Set(up _, down _, right _, left _)

  /**
   * Parses raw grid data and returns a tuple of (start, finish, set of positions)
   * @param grid a Vector of Vector of Int
   * @return a tuple (start, finish, set of positions)
   */
  def parseGrid(grid: Grid): (Option[Pos], Option[Pos], Set[Pos]) = {
    val res = for {
      y <- 0 to (grid.length - 1)
      x <- 0 to (grid(y).length - 1)
      if (grid(y)(x) == 0)
    } yield (x, y)
    (res.headOption, res.lastOption, res.toSet)
  }

  /**
   * The main solving function; it uses a recursive internal function to recursively find and count paths
   * @param start Start position
   * @param finish Goal
   * @param positions All positions of the grid
   * @return Number of ways how the goal can be reached
   */
  def countRoutes(start: Pos, finish: Pos, positions: Set[Pos]): Int = {
    def solve(currentPos: Pos, visited: Set[Pos], counter: Int): Int = {
      lazy val notVisitedNeighbours = nextPositions(currentPos, possibleMoves, positions) -- visited
      if (currentPos == finish)
        counter + 1
      else if (notVisitedNeighbours.isEmpty)
        counter
      else
        (notVisitedNeighbours foldLeft counter)((acc, nextPosition) => solve(nextPosition, visited + currentPos, acc))
    }
    solve(start, Set.empty, 0)
  }

  /**
   * Returns solution for a problem wrapped in Some (if both start and finish exist) ot None if start or finish is not found
   * @param grid a Vector[Vector[Int]] representing the grid
   * @return
   */
  def apply(grid: Grid): Option[Int] = {
    val (start, finish, positions) = parseGrid(grid)
    for ( s <- start; f <- finish ) yield countRoutes(s, f, positions)
  }

}
