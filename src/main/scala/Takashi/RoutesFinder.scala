package Takashi

/**
 *
 */
object RoutesFinder {

  type Distance = Int
  type Fare = Int

  case class Station(name: String) {
    def availableRoutes(implicit routes: Map[Station,List[(Station,Distance,Fare)]]) = routes(this)
  }

  case class Path(start: Station, goal: Station, visitedStations: List[Station], distance: Distance, fare: Fare) {
    lazy val currentStation = visitedStations.headOption.getOrElse(start)
    lazy val goalReached = visitedStations.headOption == Some(goal)
    lazy val result = (visitedStations.reverse, distance, fare)
  }

}

class RoutesFinder(input: List[(String, String, Int, Int)]) {

  import RoutesFinder._

  implicit val routes: Map[Station,List[(Station,Distance,Fare)]] = {
    val initial: Map[Station,List[(Station, Distance, Fare)]] = Map().withDefaultValue(List())
    (input foldLeft initial) {
      case (acc, (start, goal, distance, fare)) =>
        acc.updated(Station(start), (Station(goal), distance, fare)::acc(Station(start)))
    }
  }

  private def solve(policy: Ordering[Path], pathsWithoutGoal: List[Path], pathsWithGoal: List[Path] = List()): List[Path] = {

    val (newPathsWithGoal, newPathsWithoutGoal) = pathsWithoutGoal partition ( _.goalReached )

    val mergedPathsWithGoal = newPathsWithGoal ++ pathsWithGoal
    val bestPathsWithGoal =
      if (mergedPathsWithGoal.isEmpty)
        List()
      else {
        val bestPathWithGoal = mergedPathsWithGoal.minBy(path => path)(policy)
        mergedPathsWithGoal filter ( policy.equiv(_, bestPathWithGoal) )
      }

    lazy val pathsWithoutGoalWithPossiblyBetterResults =
      if (bestPathsWithGoal.isEmpty)
        newPathsWithoutGoal
      else
        newPathsWithoutGoal filter ( policy.lteq(_, bestPathsWithGoal.head) )

    if (pathsWithoutGoalWithPossiblyBetterResults.isEmpty)
      bestPathsWithGoal
    else {
      // Extend activePathsWithPossiblyBetterResults with new stations (excluding visited ones)
      val pathsWithNextStations = for {
        path <- pathsWithoutGoalWithPossiblyBetterResults
        res  <- path.currentStation.availableRoutes collect {
          case (station, distance, fare) if ! (path.visitedStations contains station) =>
            new Path(path.start, path.goal, station::path.visitedStations, path.distance + distance, path.fare + fare)
        }
      } yield res
      // Make recursive call with updated input data
      solve(policy, pathsWithNextStations, bestPathsWithGoal)
    }

  }

  def findRoutes(start: String, goal: String, policy: Ordering[Path]) = {
    val initialPath = Path(Station(start), Station(goal), List(), 0, 0)
    solve(policy, List(initialPath))
  }

  def findRoutesByDistance(start: String, goal: String) = {
    object ByDistance extends Ordering[Path] {
      def compare(path1: Path, path2: Path) = path1.distance compare path2.distance
    }
    findRoutes(start, goal, ByDistance)
  }

  def findRoutesByFare(start: String, goal: String) = {
    object ByFare extends Ordering[Path] {
      def compare(path1: Path, path2: Path) = path1.fare compare path2.fare
    }
    findRoutes(start, goal, ByFare)
  }

}
