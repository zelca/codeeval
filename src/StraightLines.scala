/**
  * See <a href="https://www.codeeval.com/open_challenges/204/">Straight Lines</a>
  */
object StraightLines extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(segments) =>
      segments.groupBy(x => x).count(_._2.size > 1)
  } foreach println

  object Input {

    // 1 2 | 1 4 | 2 3 | 3 2 | 3 4
    def unapply(line: String) = {
      val points = line.split("( \\| | )").map(_.toInt).grouped(2).map(a => (a(0), a(1))).toList
      Some(lines(points))
    }

    def lines(points: List[(Int, Int)]): List[Line] = points match {
      case Nil => Nil
      case x :: Nil => Nil
      case x :: xs => xs.map(y => line(x, y)) ++ lines(xs)
    }

    def line(p1: (Int, Int), p2: (Int, Int)) =
      if (p1._1 == p2._1) VerticalLine(p1._1)
      else {
        val a = (p1._2 - p2._2) * 1d / (p1._1 - p2._1)
        val b = p1._2 - a * p1._1
        NormalLine(a, b)
      }

    class Line

    case class VerticalLine(x: Int) extends Line

    case class NormalLine(a: Double, b: Double) extends Line

  }

}
