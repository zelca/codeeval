/**
  * See <a href="https://www.codeeval.com/open_challenges/38/">String list</a>
  */
object StringList extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines()

  lines.collect {
    case Input(count, letters) => compose(Nil, count, letters)
  } foreach {
    result => println(result.mkString(","))
  }

  def compose(res: List[Char], size: Int, letters: List[Char]): List[String] =
    if (res.size == size) res.reverse.mkString :: Nil
    else letters.flatMap(c => compose(c :: res, size, letters))

  object Input {

    // 3,pop
    def unapply(line: String) = line.split(",").toList match {
      case count :: letters :: Nil => Some(count.toInt, letters.toList.distinct.sorted)
      case _ => None
    }

  }

}
