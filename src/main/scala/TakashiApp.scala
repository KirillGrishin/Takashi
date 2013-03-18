import io.Source
import Takashi._
import Takashi.RoutesFinder.Station
import util.{Success, Try, Failure}

object TakashiApp {

  def main(args: Array[String]) {

    def gridReader(input: String): Try[Grid] =
      Try {
        for { line <- input.lines.toVector } yield
          for { x <- line.split(",").toVector } yield x.toInt
      }

    def readError(e: Throwable) { println(s"An error occurred while reading input: $e") }

    println("\nPROBLEM 1")

    val problem1 =
      """0,1,0,1,0
        |0,0,0,0,1
        |0,0,0,0,0
        |0,1,0,0,0
        |0,1,0,0,0""".stripMargin

    gridReader(problem1) match {
      case Success(grid) => RoutesCounter(grid) match {
        case Some(res) => println(s"Answer: $res")
        case _ => println("Return value was not Some[Int] (probable reason is malformed input)")
      }
      case Failure(e) => readError(e)
    }

    println("\nPROBLEM 2")

    val problem2 =
    """0,5,8,3,5,2,9,5
    |7,5,9,3,8,2,7,4
    |3,7,5,6,4,4,2,1
    |0,5,7,3,9,7,8,4
    |3,6,4,6,0,6,3,8
    |2,9,6,8,6,8,2,1
    |1,8,5,9,8,2,3,0
    |9,7,7,3,9,2,1,4""".stripMargin

    gridReader(problem2) match {
      case Success(v) => ItemsPicker(v) match {
        case Some(res) => println(s"Answer: $res")
        case _ => println("Return value was not Some[Int] (probable reason is malformed input)")
      }
      case Failure(e) => readError(e)
    }

    println("\nPROBLEM 3")

    /* val problem3 =
      """たかし家前,御湯ノ水,2,160
        |御湯ノ水,夏葉原,15,140
        |御湯ノ水,霜ヶ関,21,160
        |夏葉原,小森,18,150
        |霜ヶ関,小森,14,140
        |霜ヶ関,器川,2,30
        |器川,小森,13,120
        |器川,夏葉原,14,140
        |小森,市場前,5,140""".stripMargin*/

    val problem3 =
      """TakashiIeMae,Oyunomizu,2,160
        |Oyunomizu,Natsuhaba,15,140
        |Oyunomizu,Shimogaseki,21,160
        |Natsuhaba,Komori,18,150
        |Shimogaseki,Komori,14,140
        |Shimogaseki,Utsuwakawa,2,30
        |Utsuwakawa,Komori,13,120
        |Utsuwakawa,Natsuhaba,14,140
        |Komori,Ichibamae,5,140""".stripMargin

    val routeRegex = """(.+),(.+),(\d+),(\d+)""".r

    val read3 =
      Try {
        for { routeRegex(start, finish, distance, fare) <- problem3.lines.toList }
          yield (start, finish, distance.toInt, fare.toInt)
      }

    read3 match {
      case Success(v) => {
        val routesFinder = new RoutesFinder(v)
        val start = "TakashiIeMae"
        val goal = "Ichibamae"

        val byDistance = routesFinder.findRoutesByDistance(start,goal)

        println(s"\nBest routes byDistance from $start to $goal (totally ${byDistance.length} routes): ")

        for { (stations, distance, fare) <- byDistance } yield
          println(s"Route ${stations.map(_.name).mkString(" -> ")}; distance: $distance; fare: $fare\n")

        val byFare = routesFinder.findRoutesByFare(start,goal)

        println(s"Best routes byFare from $start to $goal (totally ${byFare.length} routes): ")

        for { (stations, distance, fare) <- byFare } yield
          println(s"Route ${stations.map(_.name).mkString(" -> ")}; distance: $distance; fare: $fare\n")

      }
      case Failure(e) => readError(e)
    }
  }

}
