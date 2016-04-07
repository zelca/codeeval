/**
  * See <a href="https://www.codeeval.com/open_challenges/182/">Longest Path</a>
  */
object LongestPath extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  val moves = List((-1, 0), (1, 0), (0, 1), (0, -1))

  lines.collect {
    case Input(matrix) =>
      (for (i <- matrix.indices; j <- matrix(i).indices) yield path((i, j), Set(matrix(i)(j)), matrix)).max
  } foreach println

  type XY = (Int, Int)

  def path(cell: XY, used: Set[Char], matrix: List[List[Char]]): Int = {
    val all = moves.map(m => (cell._1 + m._1, cell._2 + m._2))
    val filtered = all.filter(p => valid(p, matrix) && !used.contains(matrix(p._1)(p._2)))
    if (filtered.isEmpty) used.size else filtered.map(p => path(p, used + matrix(p._1)(p._2), matrix)).max
  }

  def valid(xy: XY, matrix: List[List[Char]]) =
    xy._1 >= 0 && xy._1 < matrix.size && xy._2 >= 0 && xy._2 < matrix(xy._1).size

  object Input {

    // qttiwkajeerhdgpikkeaaabwl
    def unapply(line: String) =
      Some(line.toList.grouped(scala.math.sqrt(line.length).toInt).toList)

  }

}
