import org.specs2.mutable._

class RoutesFinderSpec extends Specification {

  import Takashi.RoutesFinder
  import Takashi.RoutesFinder._

  "Takashi.RoutesFinder" should {

    val input = List(
      ("A", "B", 5, 150),
      ("A", "C", 10, 450),
      ("B", "C", 3, 220),
      ("B", "D", 15, 150),
      ("C", "D", 4, 300),
      ("Z", "X", 7, 220)
    )

    val routeFinder = new RoutesFinder(input)

    "correctly parse input data into a map" in {

      "where number of routes must be the same" in {
        ((input groupBy { case (s, _, _, _) => s }).keys).map(Station(_)) === routeFinder.routes.keys
      }

    }

    val reversedInput = input map { case (start, goal, d, f) => (goal, start, d, f) }
    val reversedRoutesFinder = new RoutesFinder(reversedInput)

    val normalByDistance = routeFinder.findRoutesByDistance("A","D")
    val normalByFare = routeFinder.findRoutesByFare("A","D")

    val reversedByDistance = reversedRoutesFinder.findRoutesByDistance("D","A")
    val reversedByFare = reversedRoutesFinder.findRoutesByFare("D","A")


  }


}
