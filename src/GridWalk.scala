/**
  * See <a href="https://www.codeeval.com/open_challenges/60/">Grid Walk</a>
  */
object GridWalk extends App {

  type Cell = (Int, Int)

  val moves: List[Cell => Cell] = List(
    cell => (cell._1 + 1, cell._2),
    cell => (cell._1, cell._2 + 1),
    cell => (cell._1, cell._2 - 1),
    cell => (cell._1 - 1, cell._2))

  val newAndValid: (Cell => Boolean) = new (Cell => Boolean) {

    val processed = scala.collection.mutable.Set.empty[Cell]

    def apply(cell: Cell) =
      if (processed.contains(cell))
        false
      else {
        processed.add(cell)
        cell match {
          case (i, j) =>
            i.abs.toString.map(_.asDigit).sum + j.abs.toString.map(_.asDigit).sum <= 19
        }
      }
  }

  println(eval(Set((0, 0)), 0))

  import scala.annotation.tailrec

  @tailrec
  def eval(cells: Set[Cell], count: Int): Int =
    if (cells.isEmpty)
      count
    else {
      val valid = cells.filter(newAndValid)
      val next = valid.flatMap(cell => moves.map(move => move(cell)))
      eval(next, count + valid.size)
    }

}