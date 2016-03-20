/**
  * See <a href="https://www.codeeval.com/open_challenges/109/">Bay bridges</a>
  */
object BayBridges extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines()

  case class Segment(x1: Double, y1: Double, x2: Double, y2: Double, a: Double, b: Double)

  val segments = lines.collect {
    case Input(segment) => segment
  }

  build(segments.toList).maxBy(_.size).foreach(println)

  def build(segments: List[(String, Segment)]): List[List[String]] = segments match {
    case Nil => List(Nil)
    case s1 :: xs =>
      val ii = xs.filter(s2 => intersect(s1._2, s2._2))
      if (ii.isEmpty) build(xs).map(s1._1 :: _) else build(xs) ++ build(xs diff ii).map(s1._1 :: _)
  }

  def intersect(s1: Segment, s2: Segment) =
    if (s1.a == s2.a) false
    else {
      val x = (s2.b - s1.b) / (s1.a - s2.a)
      x <= s1.x2.max(s1.x1).min(s2.x2.max(s2.x1)) && x >= s1.x1.min(s1.x2).max(s2.x1.min(s2.x2))
    }

  object Input {

    // 3: ([37.474858, -122.131577], [37.529332, -122.056046])
    def unapply(line: String) = line.split(": ").toList match {
      case x :: pp :: Nil =>
        val points = pp.replaceAll("[^\\d,.]", "").split(",").map(_.toDouble)
        Some(x, segment(points))
      case _ => None
    }

    def segment(points: IndexedSeq[Double]) = {
      val p1 = (points(0), points(1))
      val p2 = (points(2), points(3))
      val a = (p1._2 - p2._2) / (p1._1 - p2._1)
      val b = p1._2 - a * p1._1
      Segment(p1._1, p1._2, p2._1, p2._2, a, b)
    }

  }

}