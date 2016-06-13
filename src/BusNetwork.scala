/**
  * See <a href="https://www.codeeval.com/open_challenges/134/">A Bus Network</a>
  */
object BusNetwork extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  type BusStop = (String, String)

  lines.map(parse).map {
    case (start, finish, routes) =>
      eval(List((("", start), -12)), Int.MaxValue, finish, routes, Map())
  } foreach {
    case result if result == Int.MaxValue => println("None")
    case result => println(result)
  }

  import scala.annotation.tailrec

  @tailrec
  def eval(stops: List[(BusStop, Int)], limit: Int, finish: String, routes: Map[String, List[BusStop]], distances: Map[BusStop, Int]): Int =
    stops match {
      case Nil =>
        limit
      case _ =>
        val all = stops.flatMap(stop => routes(stop._1._2).filter {
          next =>
            val time = dist(stop, next)
            next != stop._1 && time < limit && time < distances.getOrElse(next, Int.MaxValue)
        }.map(next => (next, dist(stop, next))))
        val recalculated = all.toMap ++ distances
        val min = (limit :: all.filter(next => next._1._2 == finish).map(_._2)).min
        eval(all, min, finish, routes, recalculated)
    }

  def dist(current: (BusStop, Int), next: BusStop) = current match {
    case ((route, _), time) if route == next._1 =>
      time + 7
    case ((_, stop), time) =>
      time + 12 + (if (stop != next._2) 7 else 0)
  }

  def link(bus: String, stops: List[String]) =
    stops.sliding(2).flatMap {
      case stop1 :: stop2 :: Nil =>
        (stop1, (bus, stop1)) ::(stop1, (bus, stop2)) ::(stop2, (bus, stop1)) :: Nil
      case _ => Nil
    }

  // (2,4); R1=[1,2,3,11,12,4]; R2=[5,6,4]; R3=[1,6,7]; R4=[5,6,4]; R5=[8,6,3]
  def parse(line: String) =
    line.replaceAll("[^R\\d,;=]", "").split(";").toList match {
      case ab :: buses =>
        val points = ab.split(",")
        val routes = buses.map(_.split("=").toList).flatMap {
          case bus :: stops :: Nil => link(bus, stops.split(",").toList)
          case _ => Nil
        }.groupBy(_._1).mapValues(_.map(_._2))
        (points(0), points(1), routes)
      case _ =>
        throw new IllegalArgumentException
    }

}