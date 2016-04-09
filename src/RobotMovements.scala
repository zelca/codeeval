/**
  * See <a href="https://www.codeeval.com/open_challenges/56/">Robot Movements</a>
  */
object RobotMovements extends App {

  val moves = for (i <- -1 to 1; j <- -1 to 1 if (i + j).abs == 1) yield (i, j)

  def eval(cell: (Int, Int), visited: Set[(Int, Int)]): Int = cell match {
    case (3, 3) => 1
    case (i, j) =>
      val filtered = moves.map(x => (x._1 + i, x._2 + j)).filter(valid(_, visited))
      filtered.map(eval(_, visited + cell)).sum
  }

  def valid(xy: (Int, Int), visited: Set[(Int, Int)]) =
    xy._1 >= 0 && xy._1 <= 3 && xy._2 >= 0 && xy._2 <= 3 && !visited.contains(xy)

  println(eval((0, 0), Set()))

}