/**
  * See <a href="https://www.codeeval.com/open_challenges/79/">MineSweeper</a>
  */
object MineSweeper extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  val deltas = for (i <- -1 to 1; j <- -1 to 1 if i != 0 || j != 0) yield (i, j)

  lines.collect {
    case Input(n, m, matrix) => eval(n, m, matrix)
  } foreach (result => println(result.mkString))

  def eval(N: Int, M: Int, matrix: Array[Char]) =
    for (index <- matrix.indices; cell = (index / M, index % M); value = matrix(index)) yield
      value match {
        case '*' => "*"
        case _ =>
          val all = deltas.map(delta => (delta._1 + cell._1, delta._2 + cell._2))
          val valid = all.filter(cell => cell._1 >= 0 && cell._1 < N && cell._2 >= 0 && cell._2 < M)
          valid.count(cell => matrix(M * cell._1 + cell._2) == '*').toString
      }

  object Input {

    // 3,5;**.........*...
    def unapply(line: String) = line.split("(;|,)").toList match {
      case n :: m :: matrix :: Nil => Some(n.toInt, m.toInt, matrix.toArray)
      case _ => None
    }

  }

}
