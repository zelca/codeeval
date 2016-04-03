/**
  * See <a href="https://www.codeeval.com/open_challenges/213/">Lakes not Cakes</a>
  */
object LakesNotCakes extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  val moves = for (i <- -1 to 1; j <- -1 to 1 if !(i == j && i == 0)) yield (i, j)

  lines.collect {
    case Input(items) => eval(items, Nil)
  } foreach println

  import scala.annotation.tailrec

  @tailrec
  def eval(items: List[(String, (Int, Int))], res: List[Set[(Int, Int)]]): Int = items match {
    case Nil => res.size
    case ("o", coord) :: xs =>
      val adjacent = moves.map(move => (move._1 + coord._1, move._2 + coord._2))
      val parts = res.partition(point => adjacent.exists(point.contains))
      eval(xs, (coord :: parts._1.flatten).toSet :: parts._2)
    case _ :: xs => eval(xs, res)
  }

  object Input {

    // o # o | # # # | o # o
    def unapply(line: String) = {
      val items = line.split(" \\| ").map(_.split(" "))
      val withCoord = for (i <- items.indices; j <- items(i).indices) yield (items(i)(j), (i, j))
      Some(withCoord.toList)
    }

  }

}