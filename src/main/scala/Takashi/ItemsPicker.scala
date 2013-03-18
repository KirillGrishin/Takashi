package Takashi

import annotation.tailrec

/**
 * Finds the maximum possible number of items that can be picked from a grid going only down or right;
 */
object ItemsPicker {

  /**
   * Parses input into a map of position -> value at position; and also finds goal position at the same time
   * @param grid an array of arrays of values
   * @return a tuple of goal position and a map of position -> value at position
   */
  def parseGrid(grid: Grid): (Option[Pos], Map[Pos, Int]) = {

    val res = for {
      y <- 0 to (grid.length - 1)
      x <- 0 to (grid(y).length - 1)
    } yield ((x, y), grid(y)(x))

    val goal: Option[Pos] = res.lastOption map { case (pos, _) => pos }

    (goal, res.toMap)

  }

  /**
   * A set of functions for moving backwards - from end to beginning;
   */
  def up (pos: Pos): Pos = pos match { case (x, y) => (x, y - 1) }
  def left(pos: Pos): Pos = pos match { case (x, y) => (x - 1, y) }
  val reversedMoves = Set(up _, left _)

  def apply(grid: Grid) = {

    val (goalOption, positionsWithValues) = parseGrid(grid)

    // A set of all valid positions on the grid; required when calculating moves to prevent going out of the grid
    val positions = positionsWithValues.keySet

    /**
     * A recursive function (with TCO) that starts from the end and for each position on the grid
     * it calculates how much nuts at maximum can be picked from that position; this implementation allows
     * calculating result for big sets of data (non TCO implementation is very slow and doesn't really work for
     * big sets of data).
     *
     * @param result A list of couples containing a position and a number of nuts that
     *               can be picked up at maximum from this position
     * @return
     */
    @tailrec
    def count(result: List[(Pos, Int)]): List[(Pos, Int)] = {

      // Finds all previous positions that lead to current position and calculates the result
      // for these positions
      val previousPositions = for {
        (pos, value) <- result
        prevPos      <- nextPositions(pos, reversedMoves, positions)
      } yield (prevPos, positionsWithValues(prevPos) + value)

      // Because previousPositions may contain duplicate positions filter them and leave only ones
      // with maximum accumulated value
      val filteredRes = for {
        element@(pos, value) <- previousPositions
        elementsWithSamePosition = previousPositions filter { case (pos_, _) => pos == pos_}
        if (elementsWithSamePosition forall { case (_, value_) => value >= value_ } )
      } yield element

      if (previousPositions.isEmpty)
        result
      else
        count(filteredRes.distinct)

    }

    for {
      goal         <- goalOption
      (_, result ) <- count(List((goal, positionsWithValues(goal)))).headOption
    } yield result

  }

}
