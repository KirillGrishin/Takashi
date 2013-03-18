import org.specs2.mutable._

class ItemsPickerSpec extends Specification {

  import Takashi._
  import Takashi.ItemsPicker

  val kuriGrids = List(
    (
      Vector(
        Vector(0,5),
        Vector(7,5)
      ),
      Some((1,1))
      ),
    (
      Vector(
        Vector(0,5,8,3,5,2,9,5),
        Vector(7,5,9,3,8,2,7,4),
        Vector(3,7,5,6,4,4,2,1),
        Vector(0,5,7,3,9,7,8,4),
        Vector(3,6,4,6,0,6,3,8),
        Vector(2,9,6,8,6,8,2,1),
        Vector(1,8,5,9,8,2,3,0),
        Vector(9,7,7,3,9,2,1,4)
      ),
      Some((7,7))
      ),
    // An empty grid
    (
      Vector(),
      None
    )

  )

  kuriGrids foreach { case (grid, expectedGoal) =>

    val (goal, positionsWithValues) = ItemsPicker.parseGrid(grid)

    "takashikun.RouteFinder.KuriPicker.parseGrid" should {

      "find correct goal" in {
        goal === expectedGoal
      }

      val lengthOfPositions = ((grid foldLeft 0)((acc, element) => acc + element.length))
      val lengthOfMap = positionsWithValues.toList.length

      s"contain same count of positions ($lengthOfPositions == $lengthOfMap)" in {
        lengthOfPositions === lengthOfMap
      }

      "and caculate correct map of position -> value" in {

        positionsWithValues foreach { case ((x, y), value) =>
          grid(y)(x) === value
        }

      }

    }

    "takashikun.RouteFinder.KuriPicker.apply" should {

      // Using naive non-TCO function to calculate the result;
      // then it is compared with result returned by function in the module;

      val moves = Set(
      // move right
      { pos: Pos => pos match { case (x, y) => (x + 1, y) } },
      // move down
      { pos: Pos => pos match { case (x, y) => (x, y + 1) } }
      )

      val positions = positionsWithValues.keySet

      def naiveCounter(currentPos: Pos): Int = {

        val nextPositionElements = nextPositions(currentPos, moves, positions)

        val remainingResult =
          if (nextPositionElements.isEmpty)
            0
          else
            ( nextPositionElements map ( nextPositionElement => naiveCounter(nextPositionElement)) ).max

        remainingResult + positionsWithValues(currentPos)

      }

      val start = if (! grid.isEmpty && ! grid(0).isEmpty ) Some((0,0)) else None

      val expected = start map ( naiveCounter(_) )
      val actual = ItemsPicker(grid)

      s"return correct count of picked nuts ($expected == $actual)" in {
        expected === actual
      }

    }

  }

}
