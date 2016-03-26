/**
  * See <a href="https://www.codeeval.com/open_challenges/123/">Efficient Delivary</a>
  */
object EfficientDelivery extends App {

  val lines = scala.io.Source.fromFile(args(0)).getLines()

  lines.collect {
    case Input(tankers, oil) =>
      calc(tankers, oil, tankers.map(tanker => (tanker._2, 0)).toMap).partition(_._2 == 0)
  } foreach {
    case (Nil, result) =>
      println(result.map(_._2).max.abs)
    case (result, _) =>
      println(result.map(_._1.toList.sorted.map(_._2).mkString("[", ",", "]")).mkString)
  }

  def calc(tankers: List[(Int, Int)], oil: Int, result: Map[Int, Int]): List[(Map[Int, Int], Int)] =
    tankers match {
      case Nil => Nil
      case _ if oil <= 0 => (result, oil) :: Nil
      case x :: xs =>
        calc(xs, oil, result) ++ calc(x :: xs, oil - x._1, result + (x._2 -> (result.getOrElse(x._2, 0) + 1)))
    }

  object Input {

    // (6,9,20), 44
    def unapply(line: String) = line.split(", ").toList match {
      case tankers :: oil :: Nil =>
        Some(tankers.tail.init.split(",").map(_.toInt).zipWithIndex.toList, oil.toInt)
      case _ => None
    }

  }

}
