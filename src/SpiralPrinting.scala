/**
  * See <a href="https://www.codeeval.com/open_challenges/57/">Spiral Printing</a>
  */
object SpiralPrinting extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(n, m, data) => right((0, n - 1), (0, m - 1), data)
  } foreach (result => println(result.mkString(" ")))

  def right(x: (Int, Int), y: (Int, Int), data: Array[Array[String]]): Seq[String] = {
    if (x._1 > x._2 || y._1 > y._2) return Nil
    (y._1 to y._2).map(data(x._1)(_)) ++ down((x._1 + 1, x._2), y, data)
  }

  def down(x: (Int, Int), y: (Int, Int), data: Array[Array[String]]): Seq[String] = {
    if (x._1 > x._2 || y._1 > y._2) return Nil
    (x._1 to x._2).map(data(_)(y._2)) ++ left(x, (y._1, y._2 - 1), data)
  }

  def left(x: (Int, Int), y: (Int, Int), data: Array[Array[String]]): Seq[String] = {
    if (x._1 > x._2 || y._1 > y._2) return Nil
    (y._1 to y._2).reverse.map(data(x._2)(_)) ++ up((x._1, x._2 - 1), y, data)
  }

  def up(x: (Int, Int), y: (Int, Int), data: Array[Array[String]]): Seq[String] = {
    if (x._1 > x._2 || y._1 > y._2) return Nil
    (x._1 to x._2).reverse.map(data(_)(y._1)) ++ right(x, (y._1 + 1, y._2), data)
  }

  object Input {

    // 5;3;1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
    def unapply(line: String) = line.split(";").toList match {
      case n :: m :: data :: Nil => Some(n.toInt, m.toInt, data.split(" ").grouped(m.toInt).toArray)
      case _ => None
    }

  }

}