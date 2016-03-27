/**
  * See <a href="https://www.codeeval.com/open_challenges/51/">Closes Pair</a>
  */
object ClosestPair extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines()

  import scala.collection.mutable.ListBuffer

  val input = lines.foldRight(List.empty[ListBuffer[(Int, Int)]]) {
    (line, acc) =>
      val n = line.indexOf(' ')
      if (n >= 0) {
        acc.head += line.take(n).toInt -> line.drop(n + 1).toInt
        acc
      }
      else ListBuffer.empty[(Int, Int)] :: acc
  }.tail.map(_.toIndexedSeq)

  input.map {
    case points => combine(points).foldLeft(Long.MaxValue) {
      case (min, (p1, p2)) =>
        val x = p1._1 - p2._1
        val y = p1._2 - p2._2
        val distance = x.toLong * x + y.toLong * y
        if (distance < min) distance else min
    }
  }.map {
    case Long.MaxValue => "INFINITY"
    case min => "%1.4f" format Math.sqrt(min)
  } foreach println

  def combine(points: Seq[(Int, Int)]) =
    for (p1 <- 1 until points.size; p2 <- p1 + 1 until points.size) yield (points(p1), points(p2))

}