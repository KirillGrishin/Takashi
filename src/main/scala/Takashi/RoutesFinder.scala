package Takashi

import annotation.tailrec

/**
 * Finds a list of the best routes (one or more if there are several routes with same result). "Best" is defined
 * by an instance of Ordering[Path].
 */

object RoutesFinder {

  type Distance = Int
  type Fare = Int

  case class Station(name: String) {
    def availableRoutes(routes: Map[Station,List[(Station,Distance,Fare)]]) = routes(this)
  }

  case class Path (goal: Station, visitedStations: List[Station], distance: Distance, fare: Fare) {
    require(visitedStations.nonEmpty)
    lazy val currentStation = visitedStations.head
    lazy val goalReached = visitedStations.head == goal
  }

}

class RoutesFinder(input: List[(String, String, Int, Int)]) {

  import RoutesFinder._

  val routes: Map[Station,List[(Station,Distance,Fare)]] = {
    val initial: Map[Station,List[(Station, Distance, Fare)]] = Map().withDefaultValue(List())
    (input foldLeft initial) {
      case (acc, (start, goal, distance, fare)) =>
        acc.updated(Station(start), (Station(goal), distance, fare)::acc(Station(start)))
    }
  }

  /**
   * Main function that makes the calculation; accepts an initial path with start station in the list of
   * visited stations and recursively expands it to all available stations
   *
   * @param policy an instance of Ordering[Path]; this is used to decide how paths are compared
   * @param pathsWithoutGoal for the first call to this function this parameter is a singleton list and its single
   *                         element is a Path instance with distance and fare set to 0, and start station being
   *                         the only element in visitedStations
   * @param pathsWithGoal a list of paths where the destination station is reached; empty on the initial call
   * @return A list of paths with best result (according to policy)
   */
  @tailrec
  private def solve(policy: Ordering[Path], pathsWithoutGoal: List[Path], pathsWithGoal: List[Path] = List()): List[Path] = {

    val (newPathsWithGoal, newPathsWithoutGoal) = pathsWithoutGoal partition ( _.goalReached )

    // Find shortest paths among all paths with goal
    val mergedPathsWithGoal = newPathsWithGoal ++ pathsWithGoal
    val bestPathsWithGoal =
      if (mergedPathsWithGoal.isEmpty)
        List()
      else {
        val bestPathWithGoal = mergedPathsWithGoal.minBy(path => path)(policy)
        mergedPathsWithGoal filter ( policy.equiv(_, bestPathWithGoal) )
      }

    // Find paths which may still produce better result (e.g. with distance or fare better than current best result)
    val pathsWithoutGoalWithPossiblyBetterResults =
      if (bestPathsWithGoal.isEmpty)
        newPathsWithoutGoal
      else
        newPathsWithoutGoal filter ( policy.lteq(_, bestPathsWithGoal.head) )

    // If there are no paths shorter than the best current result, then exit the the function
    if (pathsWithoutGoalWithPossiblyBetterResults.isEmpty)
      bestPathsWithGoal
    else {
      // Extend activePathsWithPossiblyBetterResults with new stations (excluding visited ones)
      val pathsWithNextStations = for {
        path <- pathsWithoutGoalWithPossiblyBetterResults
        res  <- path.currentStation.availableRoutes(routes) collect {
          case (station, distance, fare) if ! (path.visitedStations contains station) =>
            Path(path.goal, station::path.visitedStations, path.distance + distance, path.fare + fare)
        }
      } yield res

      solve(policy, pathsWithNextStations, bestPathsWithGoal)
    }

  }

  def findRoutes(start: String, goal: String, policy: Ordering[Path]) = {
    val initialPath = Path(Station(goal), List(Station(start)), 0, 0)
    solve(policy, List(initialPath)) map { case Path(_, route, distance, fare) => (route.reverse, distance, fare)}
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
