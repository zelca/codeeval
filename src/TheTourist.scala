/**
  * See <a href="https://www.codeeval.com/open_challenges/219/">The Tourist</a>
  */
object TheTourist extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  type Distances = Map[(Int, Int), Int]

  lines.collect {
    case Input(count, distances) =>
      val cities = (1 to count).toSet
      eval(cities, Map(), distances)
  } foreach (result => println(result.getOrElse("False")))

  def eval(cities: Set[Int], paths: Map[Int, Int], distances: Map[(Int, Int), Int]): Option[Int] = cities.isEmpty match {
    case true => Some(0)
    case false => cities.map(x => (paths.getOrElse(x, 0), x)).min match {
      case (Int.MaxValue, city) =>
        None
      case (distance, city) =>
        val recalculated = (cities - city).map(x => (x, min(paths.get(x), distances.get((x.min(city), city.max(x))))))
        eval(cities - city, recalculated.toMap, distances).map(_ + distance)
    }
  }

  def min(dist1: Option[Int], dist2: Option[Int]) = (dist1, dist2) match {
    case (None, None) => Int.MaxValue
    case (None, Some(d2)) => d2
    case (Some(d1), None) => d1
    case (Some(d1), Some(d2)) => d1.min(d2)
  }

  object Input {

    import scala.annotation.tailrec

    @tailrec
    def routes(data: List[List[Int]], cities: Set[Int], distances: Distances): (Int, Distances) =
      data match {
        case Nil =>
          (cities.size, distances)
        case (c1 :: c2 :: d :: Nil) :: xs =>
          routes(xs, cities + c1 + c2, distances + ((c1.min(c2), c2.max(c1)) -> d.toInt))
        case _ :: xs =>
          routes(xs, cities, distances)
      }

    // 1 2 1 | 2 3 2 | 3 1 3
    def unapply(line: String) = {
      val data = line.split(" \\| ").toList.map(_.split(" ").toList.map(_.toInt))
      Some(routes(data, Set(), Map()))
    }

  }

}
