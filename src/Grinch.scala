/**
  * See <a href="https://www.codeeval.com/open_challenges/229/">Grinch</a>
  */
object Grinch extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(a, b, all, distances) =>
      eval(a, b, all.filterNot(_ == a), distances)
  } foreach {
    case Nil => println("False")
    case result => println(result.min)
  }

  def eval(start: String, end: String, rest: List[String], distances: Map[(String, String), Int]): List[Int] =
    if (start == end)
      0 :: Nil
    else {
      rest.collect({
        case point if point != start && distances.contains((start, point)) =>
          eval(point, end, rest.filterNot(_ == point), distances).map(_ + distances((start, point)))
      }).flatten
    }

  object Input {

    // 1 2 3, 2 8 10, 1 9 4, 8 9 2 | 2 8
    def unapply(line: String) = line.split(" \\| ").toList match {
      case routes :: points :: Nil =>
        val ab = points.split(" ")
        val distances = routes.split(", ").map(_.split(" ").toList).collect({
          case p1 :: p2 :: d :: Nil => ((p1, p2) -> d.toInt) :: ((p2, p1) -> d.toInt) :: Nil
        }).reduce(_ ++ _).toMap
        val all = distances.keys.map(_._1).toList

        Some(ab(0), ab(1), all, distances)
      case _ => None
    }

  }

}
