/**
  * See <a href="https://www.codeeval.com/open_challenges/164/">Mars Networks</a>
  */
object MarsNetworks extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(coordinates) => eval(coordinates, Map())
  } foreach (result => println(result.ceil.toInt))

  def eval(coordinates: Set[(Int, Int)], m: Map[(Int, Int), Double]): Double =
    coordinates.isEmpty match {
      case true => 0
      case false => coordinates.map(x1 => (m.getOrElse(x1, 0d), x1)).min match {
        case (dist, x1) =>
          val rest = coordinates - x1
          val distances = rest.map(x => (x, m.getOrElse(x, Double.MaxValue).min(distance(x, x1))))
          dist + eval(rest, distances.toMap)
      }
    }

  def distance(x1: (Int, Int), x2: (Int, Int)) =
    scala.math.sqrt(scala.math.pow(x1._1 - x2._1, 2) + scala.math.pow(x1._2 - x2._2, 2))

  object Input {

    // 9013,3937 7791,872 2417,3183
    def unapply(line: String) =
      Some(line.split(" ").map(_.split(",")).map(coord => (coord(0).toInt, coord(1).toInt)).toSet)

  }

}
