/**
  * See <a href="https://www.codeeval.com/open_challenges/157/">The Labyrinth</a>
  */
object TheLabyrinth extends Challenge {

  type XY = (Int, Int)

  val moves = for (i <- -1 to 1; j <- -1 to 1 if (i + j).abs == 1) yield (i, j)

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  eval(lines.map(_.toList).toList)

  def eval(maze: List[List[Char]]) = {

    val valid = for (r <- maze.indices; c <- maze(r).indices; if maze(r)(c) == ' ') yield (r, c)

    val entrances = valid.filter(cell => cell._1 == 0 || cell._2 == 0 || cell._1 == maze.size - 1 || cell._2 == maze.head.size - 1)

    val result = path(List(entrances.head), entrances.last, valid.toSet -- Set(entrances.head), Map()).toSet

    for (r <- maze.indices; c <- maze(r).indices;
         v = if (result.contains((r, c))) "+" else maze(r)(c);
         n = if (c == maze(r).size - 1) "\n" else "") print(v + n)
  }

  def path(cells: Iterable[XY], exit: XY, valid: Set[XY], res: Map[XY, XY]): List[XY] =
    if (cells.size > 1 && cells.exists(_ == exit))
      build(cells.find(_ == exit), res, Nil)
    else {
      val neighbors = (for (cell <- cells;
                            move <- moves;
                            neighbor = (cell._1 + move._1, cell._2 + move._2)
                            if valid.contains(neighbor))
        yield neighbor -> cell).toMap
      path(neighbors.keys, exit, valid -- neighbors.keys, res ++ neighbors)
    }

  def build(cell: Option[XY], res: Map[XY, XY], path: List[XY]): List[XY] = cell match {
    case None => path
    case Some(x) => build(res.get(x), res, x :: path)
  }

}
