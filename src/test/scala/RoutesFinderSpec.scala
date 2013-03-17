import org.specs2.mutable._

class RoutesFinderSpec extends Specification {

  import Takashi.RoutesFinder



  val input = List(
    ("A", "B", 5, 150),
    ("A", "C", 10, 450),
    ("B", "C", 3, 220),
    ("B", "D", 15, 150),
    ("C", "D", 4, 300),
    ("Z", "X", 7, 220)
  )

  val routeFinder = new RoutesFinder(input)
  val inputStationsMap = input groupBy { case (s, _, _, _) => s }
  val parsedStationsMap = for { (station, routes) <- routeFinder.routes } yield {
    (station.name, for { (s, d, f) <- routes } yield (station.name, s.name, d, f) )
  }

  "Takashi.RoutesFinder.routes" should {

    "be a correctly parsed a map of routes" in {

      "so that the number of start stations and their names matches " in {
        inputStationsMap.keySet === parsedStationsMap.keySet
      }

      "and each station in input has same routes as each station in parsed map" in {
        inputStationsMap foreach { case (inputStation, inputRoutes) =>
          inputRoutes.toSet === parsedStationsMap(inputStation).toSet
        }
      }

    }

  }

  val reversedInput = input map { case (start, goal, d, f) => (goal, start, d, f) }
  val reversedRoutesFinder = new RoutesFinder(reversedInput)

  val tests = List(
    ("A","D", true, { x: Int => x > 1  }),
    ("A","A", true, { x: Int => x == 1 }),
    ("A","Z", false, { x: Int => x == 0 })
  )

  "Takashi.RoutesFinder.findRoutesByDistance / findRoutesByFare" should {

    tests foreach { case (start, goal, expectsToFindRoutes, predicate) =>

      val normalByDistance = routeFinder.findRoutesByDistance(start, goal)
      val reversedByDistance = reversedRoutesFinder.findRoutesByDistance(goal, start)

      val normalByFare = routeFinder.findRoutesByFare(start, goal)
      val reversedByFare = reversedRoutesFinder.findRoutesByFare(goal, start)

      "produce expected result" in {

        "where number of found paths should be as expected (zero or non-zero)" in {
          (normalByDistance.length > 0) === expectsToFindRoutes
          (normalByFare.length > 0) === expectsToFindRoutes
        }

        "and number of stations for each found route should be as expected (0, 1 or more)" in {

          normalByDistance foreach { case (route, distance, fare) =>
            predicate(route.length) === true
          }

          normalByFare foreach  { case (route, distance, fare) =>
            predicate(route.length) === true
          }

        }

        "and have reversed result for reversed input matching the normal result" in {

          val reversedBackByDistance = reversedByDistance map { case (route, d, f) => (route.reverse, d, f) }
          normalByDistance must containTheSameElementsAs(reversedBackByDistance)

          val reversedBackByFare = reversedByFare map { case (route, d, f) => (route.reverse, d, f) }
          normalByFare must containTheSameElementsAs(reversedBackByFare)

        }

      }

    }

  }

}
