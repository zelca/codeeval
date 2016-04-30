/**
  * See <a href="https://www.codeeval.com/open_challenges/65/">Word Search</a>
  */
object WordSearch extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  val moves = for (i <- -1 to 1; j <- -1 to 1 if (i + j).abs == 1) yield (i, j)

  val board = List(
    List('A', 'B', 'C', 'E'),
    List('S', 'F', 'C', 'S'),
    List('A', 'D', 'E', 'E'))

  val matrix = board.zipWithIndex.flatMap {
    case (l, i) => l.zipWithIndex.map(x => ((i, x._2), x._1))
  }.toMap

  lines.map {
    case line if (line diff board.flatten).nonEmpty => false
    case line =>
      matrix.filter(_._2 == line.head).foldLeft(false) {
        (acc, pos) => acc || eval(pos._1, line.toList.tail, matrix - pos._1)
      }
  } foreach (res => println(res.toString.capitalize))

  def eval(pos: (Int, Int), line: List[Char], matrix: Map[(Int, Int), Char]): Boolean =
    line match {
      case Nil => true
      case x :: xs =>
        moves.foldLeft(false) {
          (acc, m) =>
            val next = (pos._1 + m._1, pos._2 + m._2)
            acc || (matrix.getOrElse(next, ' ') == x && eval(next, xs, matrix - next))
        }
    }

}
