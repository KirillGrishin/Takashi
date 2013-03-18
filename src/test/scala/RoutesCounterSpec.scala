import org.specs2.mutable._

class RoutesCounterSpec extends Specification {

  import Takashi._

  case class TestSet(
                      grid: Grid,
                      expectedStart: Option[Pos],
                      expectedFinish: Option[Pos],
                      expectedPositions: Set[Pos],
                      expectedRoutesCount: Option[Int]
                      )

  "takashikun.RouteFinder.nextPositions" should {

    val grid = Set((0,0),(0,1),(1,0),(1,1))

    val moves = Set(
    // move right
    { pos: Pos => pos match { case (x, y) => (x + 1, y) } },
    // move left
    { pos: Pos => pos match { case (x, y) => (x - 1, y) } },
    // move down
    { pos: Pos => pos match { case (x, y) => (x, y + 1) } },
    // move up
    { pos: Pos => pos match { case (x, y) => (x, y - 1) } }
    )

    val result = nextPositions((0,0), moves, grid)

    s"return a set of next positions which are inside the grid ($result in $grid)" in {
      result.subsetOf(grid) must beTrue
    }

  }

  val grids = List(
    // #1
    TestSet(
      Vector(
        Vector(0,1),
        Vector(0,0)
      ),
      Some((0,0)),
      Some((1,1)),
      Set((0,0),(0,1),(1,1)),
      Some(1)
    ),
    // #2
    TestSet(
      Vector(
        Vector(0,0,1),
        Vector(0,0,0),
        Vector(1,0,0)
      ),
      Some((0,0)),
      Some((2,2)),
      Set((0,0),(0,1),(1,0),(1,1),(1,2),(2,1),(2,2)),
      Some(4)
    ),
    // #3 with dead ends
    TestSet(
      Vector(
        Vector(0,0,0,0),
        Vector(1,0,1,0)
      ),
      Some((0,0)),
      Some((3,1)),
      Set((0,0),(1,0),(2,0),(3,0),(1,1),(3,1)),
      Some(1)
    ),
    // #4 with 0 solutions
    TestSet(
      Vector(
        Vector(0,0,0,1),
        Vector(1,0,1,0)
      ),
      Some((0,0)),
      Some((3,1)),
      Set((0,0),(1,0),(2,0),(1,1),(3,1)),
      Some(0)
    )
  )

  grids foreach { case TestSet(grid, expectedStart, expectedFinish, expectedPositions, expectedCount) =>

    val (start, finish, positions) = RoutesCounter.parseGrid(grid)

    "takashikun.RouteFinder.RoutesCounter.parseGrid" should {

      s"have start position $expectedStart" in {
        start === expectedStart
      }

      s"have finish position $expectedFinish" in {
        finish === expectedFinish
      }

      s"have a set of postions $expectedPositions" in {
        positions === expectedPositions
      }

    }

    "takashikun.RouteFinder.RoutesCounter.countRoutes" should {

      s"Return count of all routes equal to $expectedCount" in {
        //RoutesCounter.countRoutes(start.get, finish.get, positions) === expectedCount

        RoutesCounter(grid) === expectedCount

      }

    }

  }

}
