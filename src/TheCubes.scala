/**
  * See <a href="https://www.codeeval.com/open_challenges/175/">The Cubes</a>
  */
object TheCubes extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  type XYZ = (Int, Int, Int)

  val moves: Seq[XYZ => XYZ] = for (i <- -1 to 1; j <- -1 to 1; k <- -1 to 1 if i.abs + j.abs + k.abs == 1)
    yield (cell: XYZ) => (cell._1 + i, cell._2 + j, cell._3 + k)

  lines.collect {
    case Input(size, cube) =>
      val start = init(cube, size)
      path(Set(start), cube, size, 1)
  } foreach println

  def path(cells: Set[XYZ], cube: Map[XYZ, Char], size: Int, steps: Int): Int =
    cells match {
      case _ if cells.isEmpty => 0
      case _ if cells.exists(exit(_, size)) => steps
      case _ =>
        val neighbors = for (cell <- cells;
                             move <- moves;
                             next = move(cell)
                             if valid(cell, next, cube, size) && !cells.contains(next)) yield next
        path(neighbors, cube -- cells, size, steps + 1)
    }

  def exit(cell: XYZ, size: Int) =
    cell._1 == size - 1 && (cell._2 == 0 || cell._2 == size - 1 || cell._3 == 0 || cell._3 == size - 1)

  def valid(cell: XYZ, next: XYZ, cube: Map[XYZ, Char], size: Int) =
    cube.contains(next) && cube(next) != '*' && (cell._1 == next._1 ||
      (next._1 == cell._1 - 1 && cube(cell) == 'o') || (next._1 == cell._1 + 1 && cube(next) == 'o'))

  def init(cube: Map[XYZ, Char], size: Int): XYZ = {
    val borders = (for (x <- List(0, size - 1); y <- 0 until size) yield (0, x, y)) ++
      (for (x <- 0 until size; y <- List(0, size - 1)) yield (0, x, y))
    borders.find(cube(_) == ' ').get
  }

  object Input {

    // 5;******   ** ****   *** ********  o**o* ** * ************   ** * **o* ************ o **** **   ******** ***o  ****o**o  ******
    def unapply(line: String) = line.split(";").toList match {
      case n :: labyrinth :: Nil =>
        val size = n.toInt
        val all = labyrinth.toList.grouped(size * size).map(_.grouped(size).toList).toList
        val cube = for (i <- all.indices;
                        j <- all(i).indices;
                        k <- all(i)(j).indices;
                        c = all(i)(j)(k)) yield (i, j, k) -> c
        Some(size, cube.toMap)
      case _ => None
    }

  }

}
