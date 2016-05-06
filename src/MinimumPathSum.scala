/**
  * See <a href="https://www.codeeval.com/open_challenges/72/">Minimum Path Sum</a>
  */
object MinimumPathSum extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  val moves: List[Tuple2[Int, Int] => (Int, Int)] = List(
    cell => (cell._1 + 1, cell._2),
    cell => (cell._1, cell._2 + 1))

  parse(lines.toList, -1, Nil).map {
    matrix => eval(Map((0, 0) -> matrix.head.head), matrix, matrix.size)
  } foreach println

  type Row = List[Int]

  type Matrix = List[Row]

  def parse(lines: List[String], count: Int, res: Matrix): List[Matrix] = (lines, count) match {
    case (Nil, _) =>
      res.reverse :: Nil
    case (x :: xs, -1) =>
      parse(xs, x.toInt, Nil)
    case (x :: xs, 0) =>
      res.reverse :: parse(xs, x.toInt, Nil)
    case (x :: xs, _) =>
      val row = x.split(",").map(_.toInt).toList
      parse(xs, count - 1, row :: res)
  }

  import scala.annotation.tailrec

  @tailrec
  def eval(res: Map[(Int, Int), Int], matrix: Matrix, size: Int): Int =
    if (res.size == 1 && res.contains((size - 1, size - 1))) res(size - 1, size - 1)
    else {
      val all = res.toList.flatMap(x => moves.map(m => (m(x._1), x._2)).filter(x => x._1._1 < size && x._1._2 < size))
      val updated = all.foldLeft(Map[(Int, Int), Int]()) {
        case (acc, (k, value)) =>
          val min = (value + matrix(k._1)(k._2)).min(acc.getOrElse(k, res.getOrElse(k, Int.MaxValue)))
          acc + (k -> min)
      }
      eval(updated, matrix, size)
    }

}